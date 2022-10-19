# kblib

kblib is a header-only general utility library for C++14 and C++17, intended to
provide performant high-level abstractions and more expressive ways to do simple
things.

Documentation (to the extent that it has been written) can be found on [my website](https://files.fileswhatever.net/code/kblib/doc/html/).

Being header-only, there is no build process unless you want to build the tests. To install it, simply copy the 'kblib' folder into your include path. No special feature flags are required to use kblib, but the language standard must be at least C++14.

The build system for the tests is currently qmake, which is used by Qt Creator and not much else. 
