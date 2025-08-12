;;; scripts/freeze.scm

;; Copyright (C) 2025 Matthew Wette
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
  #:use-module ((system base compile) #:select (compiled-file-name))
  #:use-module ((srfi srfi-1)
                #:select (fold fold-right last every remove lset-union))
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
  (simple-format #t "Usage: guild freeze [OPTIONS] FILE
Generate a frozen script.

  -h, --help            print this help message
  --version             print version number

Note that license restrictions may apply.
  User of this script is responsible.\n"))

(define options
  ;; specification of command-line options
  ;; (option (char str) req-arg? opt-arg? proc)
  (list
   (option '(#\h "help") #f #f
           (lambda (opt name arg opts files)
             (values (acons 'help #t opts) files)))
   (option '("version") #f #f
           (lambda (opt name arg opts files)
             (show-version) (exit 0)))
   ))

;; from scripts/compile.scm
(define (parse-args args)
  (args-fold args
             options
             (lambda (opt name arg files opts)
               (fail "unrecognized option: ~S" name)
               (exit 1))
             (lambda (file opts files)
               (values opts (cons file files)))
             `() '()))


(define instccachedir (assq-ref %guile-build-info 'ccachedir))
(define userccachedir %compile-fallback-path)

(define ignore-me
  '((system syntax internal)
    (compile compile-file)
    (guile-user)))

(define *name* (make-parameter "foo"))
(define (*xd*) (string-append (*name*) ".xd"))

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
(define (probe-file filename) ;; => deps
  (call-with-input-file filename
    (lambda (port)
      (let loop ((deps '()) (exp (read port)))
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
    (cons mod-spec (probe-file (module-filename mod-spec))))
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
  (define (try head tail ext)
    (let ((path (string-append head "/" tail ext)))
      (and (access? path R_OK) path)))
  (or (try instccachedir path ".go")
      (try userccachedir path ".scm.go")
      (and=> (canize-path (string-append path ".scm"))
             (lambda (path) (try userccachedir path ".go")))
      (error (string-append path " .go file not found"))))

;; script -> ((scm-file . go-path) (scm-file . go-path) ...)
(define (find-gos script)
  (let* (
         ;; get the boot-9 files : (needed anymore?)
         (bootd (get-dict '((ice-9 boot-9))))
         (boots (apply lset-union equal? bootd))
         (bootseq (reverse (tsort bootd boots)))
         ;;
         (depd (get-dict (list '(mydemo1))))
         (all (apply lset-union equal? depd))
         (seq (reverse (tsort depd all)))
         ;;
         (userseq (remove (lambda (e) (member e boots)) seq))
         (seq (append bootseq userseq))
         ;;
         (scmfl (map
                 (lambda (m) (string-append
                              (string-join (map symbol->string m) "/") ".scm"))
                 seq))
         (basel (map (lambda (m) (string-join (map symbol->string m) "/")) seq))
         (gopl (map search-compiled-path basel))
         (inc-sys-gos #f))
    (fold-right
     (lambda (scmf gop seed)
       (if (or
            inc-sys-gos
            (not (string-prefix? instccachedir gop)))
           (cons (cons scmf gop) seed)
           seed))
     '() scmfl gopl)))

;; ----------------------------------------------------------------------------

(define B-map
  '(("x86_64" . "i386")
    ))

(define O-map
  '(("x86_64" . "elf64-x86-64")
    ))

;; a bit kludgy, for now
(define* (genxo gopath xofile)
  (let ((wd (getcwd))
        (gofile (string-append (basename xofile ".o") ".go"))
        (objcopy "objcopy")
        (march "x86_64")
        )
    (chdir (*xd*))
    (when (access? gofile R_OK) (delete-file gofile))
    (symlink gopath gofile)
    (system 
     (simple-format
      #f
      "~a -I binary -B ~a -O ~a --add-section .note.GNU-stack=/dev/null ~a ~a"
      objcopy (assoc-ref B-map march) (assoc-ref O-map march) gofile xofile))
    (delete-file gofile)
    (chdir wd)))

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

(define (gen-xos go-pairs)
  (unless (access? (*xd*) 7) (system (string-append "mkdir " (*xd*))))
  (let* ((go-refs (map car go-pairs))
         (go-paths (map canize-path (map cdr go-pairs))))
    (map
     (lambda (ref gopath)
       (let* ((rfil (basename gopath ".go"))
              (rdir (dirname gopath))
              (rhead (string-append rdir "/" rfil))
              (rhash (hash-path rdir))
              (cfil (sanitize-name rfil))
              (xbase (string-append rhash "_" cfil))
              (xofile (string-append xbase ".o")))
         (genxo gopath xofile)
         (cons ref xbase)))
     go-refs go-paths)))

(define code-part1 "
#include <libguile.h>

SCM scm_load_thunk_from_memory(SCM);

static SCM zcm_c_pointer_to_bytevector(void *pointer, size_t size) {
  SCM ptr, len, mem;

  ptr = scm_from_pointer(pointer, NULL);
  len = scm_from_size_t(size);
  mem = scm_pointer_to_bytevector(ptr, len, SCM_UNDEFINED, SCM_UNDEFINED); 
  return mem;
}

void loadem() {
  char *ptr, *end;
  size_t siz;
  SCM mem, res, mod_init;\n\n")

(define (code-part2a term)
  (string-append
   (simple-format #f "  ptr = _binary_~a_go_start;\n" term)
   (simple-format #f "  end = _binary_~a_go_end;\n" term)))

(define code-part2b
  "  siz = end - ptr;
  mem = zcm_c_pointer_to_bytevector (ptr, siz);
  mod_init = scm_load_thunk_from_memory(mem);
  res = scm_call_0(mod_init);\n\n")

(define code-part3
  "  return;\n}\n")

;; @deffn {Procedure} gen-ci xpairs
;; Generage a C file to load .go files from memory.
;; @end deffn
(define (gen-ci xpairs)
  (let ((sport (open-output-file (string-append (*name*) ".c"))))
    (simple-format sport "/* ~a.c\n */\n\n#include <libguile.h>\n\n" (*name*))
    (for-each
     (lambda (xpair)
       (let* ((ref (car xpair))
              (xbase (cdr xpair))
              (ebase (string-append xbase "_go")))
         (simple-format sport "/* ~a */\n" ref)
         (simple-format sport "extern char _binary_~a_start[];\n" ebase)
         (simple-format sport "extern char _binary_~a_end[];\n" ebase)))
     xpairs)
    (display code-part1 sport)
    (for-each
     (lambda (xpair)
       (let ((ref (car xpair)) (xbase (cdr xpair)))
         (simple-format sport "  /* ~a */\n" ref)
         (display (code-part2a xbase) sport)
         (display code-part2b sport)))
     xpairs)
    (display code-part3 sport)
    (close-port sport)))


;; ----------------------------------------------------------------------------

(define (main . args)
  (call-with-values (lambda () (parse-args args))
    (lambda (opts files)
      (when (or (assq-ref opts 'help) (null? files)) (show-usage) (exit 0))
      (let* ((namefile (last files))
             (base (basename namefile ".scm")))
        (*name* base))
      (for-each
       (lambda (file) (gen-ci (gen-xos (find-gos file))))
       (reverse files))))
  (exit 0))

;; --- last line ---
