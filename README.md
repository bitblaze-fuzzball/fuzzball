FuzzBALL is a symbolic execution tool for x86 (and a little ARM)
binary code, based on the BitBlaze Vine library. (The name comes from
the phrase "FUZZing Binaries with A Little Language", where "fuzzing"
is a common application of symbolic execution to bug-finding, and the
"little language" refers to the Vine intermediate language that
FuzzBALL uses for execution.  Also "fuzzball" is a common nickname for
a small kitten, and FuzzBALL was intended to be simpler and
lighter-weight than some other symbolic execution tools.)

At a high level, there are two kinds of code you can run FuzzBALL
on. First, there is any code that can execute stand-alone, without the
services of an OS or special hardware devices; this can include a
subset of code from a larger program that does need those
things. Second, there are single-threaded Linux programs, which
FuzzBALL can run by passing their system calls onto your real OS.

FuzzBALL is free software distributed under the GNU GPL: see the files
LICENSE and COPYING for details.

Compilation instructions are in the file INSTALL.

The README file includes some more detailed description of FuzzBALL
and some tutorial-style examples.

FuzzBALL's page on the Berkeley web site, at

http://bitblaze.cs.berkeley.edu/fuzzball.html

has links to some papers that build on FuzzBALL.

We are interested in your comments, questions, and feedback about
FuzzBALL via the bitblaze-users mailing list (hosted by Google Groups):

http://groups.google.com/group/bitblaze-users

