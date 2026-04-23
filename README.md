# guile-freezer - a work in progress

Guile-freezer is a guild script to convert a script and dependent modules 
to objects to link into a C program linked with libguile.

Usage:
```
  $ guild freeze [options] [files]
```
Where options are
* `-b`, `--inc-system` : include any needed system .go files
* `-s`, `--add-shell`  : execute repl-shell in the program
* `-X`, `--no-compile` : don't try to compile and load the program

For the `-b` option to work your guile must be built with the included
patch.  This allows the program to boot using the loaded boot-9.go file.

# Examples
```
  $ cd demos
  $ ./build-world
  $ ./world
```

The above will generate a `world.c` main program file and a number
of object files: one for each .go files used in the program.  The .o
files provide in-memory copies of the associated .go files so that
modules may be loaded using `scm_load_thunk_from_memory`.

# Notes

+ This is in development: alpha release
+ Only x86_64: if you want others check Omap and Bmap in freeze.scm

## hand compile
```
$ guild freeze -b -s -X world.scm
$ gcc -o world `pkg-config --cflags guile-3.0` world.c world.xd/*.o \
               `pkg-config --libs guile-3.0` 
```




