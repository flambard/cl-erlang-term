cl-erlang-term
==============

Version: 0.2.2

cl-erlang-term is a Common Lisp library for encoding and decoding objects in the
[Erlang External Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html).


How to install
--------------

Use [Quicklisp](http://www.quicklisp.org/) to install cl-erlang-term.

    > (ql:quickload :erlang-term)

alternatively

    > (ql:quickload :erlang-term-optima)

if you wish to include the [optima](https://github.com/m2ym/optima) extensions
for pattern matching on Erlang objects.

### Dependencies

- [Alexandria](http://common-lisp.net/project/alexandria/)
- [IEEE-Floats](http://common-lisp.net/projects/ieee-floats/)
- [nibbles](http://method-combination.net/lisp/nibbles/)
- [zlib](http://common-lisp.net/projects/zlib/)

Optional dependencies:

- [FiveAM](http://common-lisp.net/project/fiveam/) (unit-tests)
- [optima](https://github.com/m2ym/optima) (pattern matching)

### How to run the unit-tests

    > (ql:quickload :erlang-term-test)
    ...
    > (erlang-term-test:run-all-tests)
