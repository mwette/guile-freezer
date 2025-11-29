# guile-freezer
Guile-freezer is a guild script to convert a script and dependent modules 
to objects to link into a C program linked with libguile.

Usage:
```
  $ guild freeze myscript.scm
  $ gcc -o myscript `pkg-config --cflags guile-3.0` myscript.c myscript.xd/*.o \
                    `pkg-config --libs guile-3.0`
  $ ./myscript
```

`myscript.c` will load .go's from the objects you see in `myscript.xd`.
The default behavior of the freezer is to no include Guile native .gos.
To add these, one would use the `-s` flag, as:

```
  $ guild freeze -s myscript.scm
```

But this does not work.  Ideally one would be able to generate a
single binary with no other dependencies.  However, libguile loads
several system .go files (e.g., boot-9.go) during initialization,
which, by default, are not loaded in the binary genrated with the
freezer.  The flag to include these is `-s`.  However, this does not
work.  Feel free to dig into the guile sources (e.g., libguile/init.c)
to figure out how to make that work.  Following that, one might want
to discover how to link libguile statically.  This is all work to go.

# Notes

+ This is in development: alpha release
+ Only x86_64: if you want others check Omap and Bmap in freeze.scm

## usage
```
$ guild freeze myscript.scm
$ gcc -o myscript `pkg-config --cflags guile-3.0` myscript.c myscript.xd/*.o \
                  `pkg-config --libs guile-3.0` 
```

## install example, if your install is in /usr/local/
```
  # sudo mkdir -p /usr/local/share/guile/site/scripts
  # sudo cp scripts/freeze.scm /usr/local/share/guile/site/scripts/
```



