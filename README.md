# Overview

This repository contains the earliest known source code to the C compiler
for ancient UNIX systems, back when the C language was being developed.

These sources were taken from other repositories of early UNIX.  The
compiler files were extracted and moved into the same folder, so that
they can be diffed more easily.

See https://www.bell-labs.com/usr/dmr/www/primevalC.html for some historical
context from the primary author, Dennis Ritchie.

# Versions

There are 5 versions here, which are git tagged as-is:

* v2, also known as last1120c.  From July 1972.

* v3, also known as prestruct-c.  From December 1972.  This is only a snapshot;
the final v3 release was in February 1973.  Note v3 is missing the assembler
tables.

* v5.  Released in June 1974.  Note there are no known sources of v4.

* v7.  Released in January 1979.

* 32v.  Also released in 1979, but for the 32-bit VAX.

# Annotations

In addition, there are annotated branches of these, where I have added my
own comments as I have been trying to understand the code in detail.
These C comments all begin with "BCD:" so there is no confusion about
which comments are mine.

Most of the analysis started with v5, as that was the first version to
use the preprocessor, which allowed replacing hundreds of magic constants
with more understandable symbolic names.  Many of those comments
were then copied back onto the earlier v2 and v3 versions.

A complete analysis is not my goal, but rather to understand when various
constructs arose, and to point out other historically significant things.

