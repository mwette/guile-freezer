;;; scripts/freeze.scm

;; Copyright (C) 2025-2026 Matthew Wette
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Author: Matt Wette <matt.wette@gmail.com>

;;; Commentary:
;; Usage: freeze [options] script.scm

;;; Code:

(define-module (scripts freeze)
  
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module ((system base compile) 
               #:select (compiled-file-name compile-file))
  #:use-module ((srfi srfi-1)
                #:select (any fold fold-right last every remove lset-union))
  #:use-module (srfi srfi-37)
  #:version (0 0 1))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define (sf fmt . args) (apply simple-format #t fmt args))

(define %summary
  "Generate binary version of non-Guile modules.")

(define (fail fmt . args)
  (simple-format  (current-error-port) "compile-ffi: error: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (warn fmt . args)
  (simple-format  (current-error-port) "compile-ffi: warning: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (note fmt . args)
  (simple-format  (current-error-port) "compile-ffi: notice: ")
  (apply simple-format (current-error-port) fmt args)
  (newline (current-error-port)))

(define (show-version)
  (display "0.0\n"))

(define (show-usage)
  (simple-format #t "Usage: guild freeze [OPTIONS] FILE ...
Generate a frozen script.

  -b, --include-sys     include base system files (NOT WORKING)
  -s, --add-shell       generate repl shell
  -x, --compile         try to compile & load the program
  -h, --help            print this help message
  --version             print version number

Note that license restrictions may apply.
  User of this script is responsible.\n"))

(define options
  ;; specification of command-line options
  ;; (option (char str) req-arg? opt-arg? proc)
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg opts)
             (acons 'help #t opts)))
   (option '(#\b "include-sys") #f #f
           (lambda (opt name arg opts)
             (acons 'include-sys #t opts)))
   (option '(#\s "add-shell") #f #f
           (lambda (opt name arg opts)
             (acons 'add-shell #t opts)))
   (option '(#\x "compile") #f #f
           (lambda (opt name arg opts)
             (acons 'compile #t opts)))
   (option '("version") #f #f
           (lambda (opt name arg opts)
             (show-version) (exit 0)))
   ))

;; from scripts/compile.scm
(define (parse-args args)
  (args-fold args
             options
             (lambda (opt name arg opts)
               (fail "unrecognized option: ~S" name))
             (lambda (name opts)
               ;;(if (assq-ref opts 'file) (fail "only one file"))
               (acons 'files (cons name (assq-ref opts 'files)) opts))
             `((files))))

;; ----------------------------------------------------------------------------

(define instccachedir (assq-ref %guile-build-info 'ccachedir))
(define userccachedir %compile-fallback-path)

(define startup-modules
  '((ice-9 boot-9) (ice-9 psyntax-pp)))
(define startup-paths
  '("ice-9/boot-9.scm" "ice-9/psyntax-pp.scm"))

(define ignore-me
  '((system syntax internal)
    (compile compile-file)
    (ice-9 syntax)
    (ice-9 ports internal)
    (guile-user)))

;;(define (*xd*) (string-append (*name*) ".xd"))

;; ----------------------------------------------------------------------------

;; findgos

;; @deffn {Procedure} module-filename mod-spec
;; Given a module specification (e.g., @code{(foo bar)}) generate the
;; path to the file.
;; @end deffn
(define (module-filename mod-spec)
  (fold
   (lambda (pfix path)
     (if path path
         (let ((path
                (string-append
                 pfix "/" (string-join (map symbol->string mod-spec) "/")
                 ".scm")))
           (and (access? path R_OK) path))))
   #f %load-path))

(define (spec-dep spec seed)
  (let ((sdl (match spec (((sdl ...) . _0) sdl) ((sdl ...) sdl))))
    (if (member sdl ignore-me) seed (cons sdl seed))))

(define (mod-deps exp seed)
  (let loop ((deps seed) (tail (cddr exp)))
    (match tail
      ('() deps)
      (`(#:use-module ,spec . ,rest)
        (loop (spec-dep spec deps) rest))
      (`(#:autoload ,spec ,procs . ,rest)
       (loop (spec-dep spec deps) rest))
      ((key val . rest)
       (loop deps rest)))))

;; @deffn {Procedure} probe-file filename
;; Generate a list of dependencies for filename.
;; NOTE: This does not search for indirect methods like @ and @@.
;; @end deffn
(define* (probe-file filename #:optional (seed '())) ;; => deps
  (call-with-input-file filename
    (lambda (port)
      (let loop ((deps seed) (exp (read port)))
	(match exp
	  ((? eof-object?) (reverse deps))
          (`(define-module . ,_0) (loop (mod-deps exp deps) (read port)))
          (`(use-modules . ,spks) (loop (fold spec-dep deps spks) (read port)))
          (__ (loop deps (read port))))))))

;; @deffn {Procedure} xxx modules =>
;; Generate a dictionary of mod-spec to go-filename.
;; @end deffn
(define (get-dict modules)
  (define (mod-entry mod-spec)
    (let ((filename (module-filename mod-spec)))
      (unless filename (error "freezer: module code not found" mod-spec))
      (cons mod-spec (probe-file (module-filename mod-spec)))))
  (let loop ((dict '()) (todo modules))
    (cond
     ((null? todo)
      (reverse dict))
     ((assoc-ref dict (car todo))
      (loop dict (cdr todo)))
     (else
      (let ((entry (mod-entry (car todo))))
        (loop (cons entry dict) (append (cdr entry) todo)))))))

;; need doc
(define (tsort filed filel)
  (define (covered? deps done) (every (lambda (e) (member e done)) deps))
  (let loop ((done '()) (hd '()) (tl filel))
    (if (null? tl)
        (if (null? hd) done (loop done '() hd))
        (cond
         ((not (assq-ref filed (car tl)))
          (loop (cons (car tl) done) hd (cdr tl)))
         ((covered? (assq-ref filed (car tl)) done)
          (loop (cons (car tl) done) hd (cdr tl)))
         (else
          (loop done (cons (car tl) hd) (cdr tl)))))))

(define (canize-path path)
  (false-if-exception (canonicalize-path path)))

(define (search-compiled-path path)
  (define (sans-scm path)
    (if (string=? ".scm" (string-take-right path 4))
        (string-drop-right path 4)
        path))
  (define (try head tail ext)
    (let ((path (string-append head "/" tail ext)))
      (and (access? path R_OK) path)))
  (or (try instccachedir (sans-scm path) ".go")
      (try userccachedir path ".go")
      (and=> (canize-path path)
        (lambda (path) (try userccachedir path ".go")))
      (any (lambda (folder)
             (or (try folder (sans-scm path) ".go")
                 (and=> (canize-path (sans-scm path))
                        (lambda (path) (try folder path ".go")))))
           (string-split (or (getenv "GUILE_LOAD_COMPILED_PATH")
                             "")
                         #\:))
      (error (string-append path " .go file not found"))))

;; script -> ((scm-file . go-path) (scm-file . go-path) ...)
(define* (find-gos files #:optional (options '()))
  (let* (
         ;; collect the startup files 
         (bootd (get-dict startup-modules))
         (bootl (apply lset-union equal? bootd))
         (bootseq (reverse (tsort bootd bootl)))
         ;;
         (depd (get-dict (fold probe-file '() files)))
         (depl (apply lset-union equal? depd))
         (depseq (reverse (tsort depd depl)))
         (depseq (remove (lambda (m) (member m bootl)) depseq))
         ;;
         (seq (append bootseq depseq))
         ;;
         (srcfl (fold
                 (lambda (m l)
                   (let ((base (string-join (map symbol->string m) "/")))
                     (cons (string-append base ".scm") l)))
                 files seq))
         (gopl (map search-compiled-path srcfl)))
    (fold-right
     (lambda (scmf gop seed)
       (if (or
            (assq-ref options 'include-sys)
            (not (string-prefix? instccachedir gop)))
           (cons (cons scmf gop) seed)
           seed))
     '() srcfl gopl)))

;; ----------------------------------------------------------------------------

(define B-map
  '(("x86_64" . "i386")
    ))

(define O-map
  '(("x86_64" . "elf64-x86-64")
    ))

(define (hash-path path)
  ;; 5 base16 chars based on 24 bit hash
  (define (C16 ix) (string-ref "ABCDEFGHJKMNPRST" ix))
  (define (finish hv)
    (list->string
     (let lp ((l '()) (v hv) (i 5)) ;; i <= 6
       (if (zero? i) l
           (lp (cons (C16 (remainder v 16)) l) (quotient v 16) (1- i))))))
  (define (lnot24 x)
    (let ((v (lognot x)))
      (if (negative? v) (+ v 16777216) v)))
  (let loop ((hv 0) (ix 0))
    (if (= ix (string-length path)) (finish hv)
        (let* ((cv (char->integer (string-ref path ix)))
               (hv (logand (+ (ash hv 3) cv) #xffffff))
               (hi (ash hv -21)))
          (loop (if (zero? hi) hv (lnot24 (logxor hv (ash hi -18))))
                (1+ ix))))))

(define (sanitize-name path)
  (string-map (lambda (ch) (if (memq ch '(#\- #\.)) #\_ ch)) path))

;; a bit kludgy, for now
(define* (genxo gopath xofile xd)
  (let ((wd (getcwd))
        (gofile (string-append (basename xofile ".o") ".go"))
        (objcopy "objcopy")
        (march "x86_64")
        )
    (chdir xd)
    (when (access? gofile R_OK) (delete-file gofile))
    (symlink gopath gofile)
    (system 
     (simple-format
      #f
      "~a -I binary -B ~a -O ~a --add-section .note.GNU-stack=/dev/null ~a ~a"
      objcopy (assoc-ref B-map march) (assoc-ref O-map march) gofile xofile))
    (delete-file gofile)
    (chdir wd)))

(define (gen-xos go-pairs options)
  (let* ((name (assq-ref options 'name))
         (xd (string-append name ".xd"))
         (go-refs (map car go-pairs))
         (go-paths (map canize-path (map cdr go-pairs))))
    (unless (access? xd 7) (system (string-append "mkdir " xd)))
    (map
     (lambda (ref gopath)
       (let* ((rfil (basename gopath ".go"))
              (rdir (dirname gopath))
              (rhead (string-append rdir "/" rfil))
              (rhash (hash-path rdir))
              (cfil (sanitize-name rfil))
              (xbase (string-append rhash "_" cfil))
              (xofile (string-append xbase ".o")))
         (genxo gopath xofile xd)
         (cons ref xbase)))
     go-refs go-paths)))

(define file-header
"#include <libguile.h>

SCM scm_load_thunk_from_memory(SCM);

static SCM zcm_c_pointer_to_bytevector(void *pointer, size_t size) {
  SCM ptr, len, mem;

  ptr = scm_from_pointer(pointer, NULL);
  len = scm_from_size_t(size);
  mem = scm_pointer_to_bytevector(ptr, len, SCM_UNDEFINED, SCM_UNDEFINED); 
  return mem;
}\n")

(define load-header
  "  char *ptr, *end;
  SCM mem, res, mod_init;\n\n")

(define (gen-ptr+end term)
  (string-append
   (simple-format #f "  ptr = _binary_~a_go_start;\n" term)
   (simple-format #f "  end = _binary_~a_go_end;\n" term)))

(define load-calls
  "  mem = zcm_c_pointer_to_bytevector (ptr, end - ptr);
  mod_init = scm_load_thunk_from_memory(mem);
  res = scm_call_0(mod_init);\n\n")

(define code-part6
  "
  scm_boot_guile (argc, argv, local_main, 0);
  return 0;
}\n\n")

;; @deffn {Procedure} gen-ci xpairs
;; Generage a C file to load .go files from memory.
;; @end deffn
(define* (gen-ci xpairs #:optional (options '()))
  (let* ((script (assq-ref options 'script))
         (name (assq-ref options 'name))
         (sport (open-output-file (string-append name ".c")))
         ;;(boot9-pair (assoc "ice-9/boot-9.scm" xpairs))
         (startup-pairs (filter (lambda (pair) (member (car pair) startup-paths))
                                xpairs))
         (script-pair (assoc script xpairs)))
    (simple-format sport "/* ~a.c\n */\n" name)
    (display file-header sport)
    (display "\n\n" sport)
    (for-each
     (lambda (xpair)
       (let* ((ref (car xpair))
              (xbase (cdr xpair))
              (ebase (string-append xbase "_go")))
         (simple-format sport "/* ~a */\n" ref)
         (simple-format sport "extern char _binary_~a_start[];\n" ebase)
         (simple-format sport "extern char _binary_~a_end[];\n" ebase)))
     xpairs)
    (display "\n\n" sport)
    ;;
    (format sport "static void loadem() {\n")
    (display load-header sport)
    (for-each
     (lambda (xpair)
       (let ((ref (car xpair)) (xbase (cdr xpair)))
         (unless (member ref (cons script startup-paths))
           (simple-format sport "  /* ~a */\n" ref)
           (display (gen-ptr+end xbase) sport)
           (display load-calls sport))))
     xpairs)
    (let ((ref (car script-pair)) (xbase (cdr script-pair)))
      (simple-format sport "  /* ~a */\n" ref)
      (display (gen-ptr+end xbase) sport)
      (display load-calls sport))
    (display "  return;\n}\n\n" sport)
    ;;
    (when (assq-ref options 'include-sys)
      (format sport "static void load_boot9() {\n")
      (display load-header sport)
      (for-each
       (lambda (xpair)
         (let ((ref (car xpair)) (xbase (cdr xpair)))
           (simple-format sport "  /* ~a */\n" ref)
           (display (gen-ptr+end xbase) sport)
           (display load-calls sport)))
       startup-pairs)
      (display "  return;\n}\n\n" sport))
    ;;
    (display "static void local_main(void *data, int argc, char **argv) {\n"
             sport)
    (display "  SCM v = scm_c_public_lookup(\"guile\"," sport)
    (display " \"%load-should-auto-compile\");\n" sport)
    (display "  SCM_VARIABLE_SET(v, SCM_BOOL_F);\n" sport)
    (display "  loadem();\n" sport)
    (if (assq-ref options 'add-shell)
        (display "  scm_shell(argc, argv);\n" sport))
    (display "}\n\n" sport)
    ;;
    (display "int main(int argc, char **argv) {\n" sport)
    (if (assq-ref options 'include-sys)
        (display "  scm_load_startup_using(load_boot9);\n" sport))
    (display "  scm_boot_guile (argc, argv, local_main, 0);\n" sport)
    (display "  return 0;\n}\n\n" sport)
    ;;
    (format sport "//  gcc -o ~a `pkg-config --cflags guile-3.0` ~a.c \\\n"
            name name)
    (format sport "//    ~a.xd/*.o `pkg-config --libs guile-3.0`\n" name)
    (close-port sport)))


;; ----------------------------------------------------------------------------

(use-modules ((system base compile) #:select (compile-file)))

(define compile-format
  (string-append
   "gcc -o ~a `pkg-config --cflags guile-3.0`"
   " ~a.c ~a.xd/*.o `pkg-config --libs guile-3.0`"))

(define (main . args)
  (define opts (parse-args args))
  (when (or (assq-ref opts 'help) (not (assq-ref opts 'files)))
    (show-usage) (exit 0))
  (let* ((files (reverse (assq-ref opts 'files)))
         (script (car files))
         (name (basename script ".scm"))
         (opts (cons* `(name . ,name) `(script . ,script) opts)))
    (for-each
     (lambda (file) (compile-file file #:from 'scheme #:to 'bytecode))
     files)
    (gen-ci (gen-xos (find-gos files opts) opts) opts)
    (when (assq-ref opts 'compile)
      (system (apply format #f compile-format name name name '())))
    0))

;; --- last line ---
