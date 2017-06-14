Tiger Compiler in ML for x64:
============================

Tiger Compiler from "Modern Compiler Implementation in ML" by Andrew W. Appel.

Implemented as part of the course "Compilers" (Licenciatura en Ciencias de la
Computación - UNR).

Build
=====

To build the compiler, run:

```
make all
```

It will build the project "out of source" in a new directory: "build".

To run the tests:

```
make test
```

The runtime library: "libtigerruntime.a", must be moved to a standard location,
so the linker can find it. The easy solution is to add the "build" directory to
the list of dirs in "LIBRARY_PATH".
