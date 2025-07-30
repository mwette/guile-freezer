# guile-freezer
Guile-freezer is a guild script to convert a script and dependent modules 
to objects to link into a C program linked with libguile.

This is in development: alpha release

## usage
```
$ guild freeze myscript.scm
$ gcc -c myscript.c
$ gcc -o myprog myprog.c myscript.o myscript.xd/*.o -lguile ...
```

## install example
```
# cp scripts/freeze.scm /usr/local/share/guile/site/
```

