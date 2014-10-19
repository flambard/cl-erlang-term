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


API
---

### Encoding/decoding

[Function]  
__decode__ bytes &key start version-tag => erlang-term, position

> Decodes the sequence _bytes_ into an Erlang term, starting from position
> _start_. If _version-tag_ has an integer value, _bytes_ is expected to begin
> with a version tag byte of that value.
> 
> _start_ defaults to `0`. _version-tag_ defaults to `131`.
> 
> _position_ is the location in _bytes_ where the decoding ended.

[Function]  
__encode__  term &key version-tag compressed => bytes

> Encodes _term_ into a sequence of bytes. If _version-tag_ has an integer
> value, _bytes_ will start with a version tag byte of that value. If
> _compressed_ is _true_, the encoded term will be zlib-compressed.
> 
> _version-tag_ defaults to `131`.
> _compressed_ defaults to _false_.


### Erlang object functions

#### Creating Erlang objects

[Function]  
__binary__ &rest bytes => binary

> Creates a new Erlang binary with the contents of _bytes_.

[Function]  
__binary-to-string__ binary => string

> Converts _binary_ to a string.

[Function]  
__bytes-to-binary__ bytes => binary

> Creates a new Erlang binary with the contents from the byte sequence _bytes_.

[Function]  
__make-atom__ string => atom

> Creates a new Erlang atom (a symbol) with the name of _string_. The symbol is
> interned in the package designated by `*atom-symbol-package*`.

[Function]  
__make-pid__ node id serial creation => pid

> Creates a new Erlang pid identified by the values of _node_, _id_, _serial_,
> and _creation_.

[Function]  
__make-port__ node id creation => port

> Creates a new Erlang port identified by the values of _node_, _id_, and
> _creation_.

[Function]  
__make-reference__ node id creation => reference

> Creates a new Erlang reference identified by the values of _node_, _id_, and
> _creation_.

[Function]  
__string-to-binary__ string => binary

> Creates a new Erlang binary with the contents of _string_ converted to bytes.

[Function]  
__tuple__ &rest erlang-translatable-objects => tuple

> Creates a new Erlang tuple with _erlang-translatable-objects_ as elements.


#### Erlang object accessors

[Reader]  
__bytes__ binary => byte-vector

> Reader method for getting the contents of _binary_ as byte vector.

[Reader]  
__bits-in-last-byte__ bit-binary => number-of-bits

> Reader method for getting the number of bits in the last byte of _bit-binary_.

[Reader]  
__arity__ fun-or-tuple => arity

> Reader method for getting the arity of either an Erlang fun or an Erlang
> tuple. The arity of a fun is the number of arguments it takes. The arity of a
> tuple is the number of elements in it.

[Reader]  
__size__ binary-or-tuple => size

> Reader method for getting the size of either an Erlang binary or an Erlang
> tuple. The size of a binary is the number of bytes in it. The size of a tuple
> is the number of elements in it.

[Reader]  
__elements__ tuple => vector

> Reader method for getting the elements of _tuple_ as a vector.

[Function]  
__tuple-arity__ tuple => arity

> Function for getting the number of elements in a tuple.

[Function]  
__tuple-ref__ tuple pos => element

> Function for getting the element in _tuple_ at position _pos_.

[Reader]  
__node__ identifier => node-name

> Reader method for getting the node name from _identifier_, which is an Erlang
> pid, port, or reference.

[Reader]  
__module__ fun => module-name

> Reader method for getting the module name from an Erlang fun.


#### Generic Erlang object functions

[Generic function]  
__match-p__ object-a object-b => boolean

> Returns _true_ if _object-a_ and _object-b_ match in the Erlang sense, i.e.
> they are structurally equivalent. Otherwise returns _false_.


### Variables

[Constant variable]  
__+protocol-version+__

> The protocol version byte tag used in the Erlang External Term Format.
> 
> Value: `131`

[Special variable]  
__\*atom-symbol-package\*__

> The package in which atom symbols are interned. Symbols are uninterned if
> `NIL`.
> 
> Default value: `:keyword`

[Special variable]  
__\*erlang-false-is-lisp-nil\*__

> Treat the Erlang atom `false` as the Lisp symbol `NIL` instead of `'|false|`.
> 
> Default value: _false_

[Special variable]  
__\*erlang-string-is-lisp-string\*__

> Treat Erlang strings as Lisp strings instead of lists of integers.
> 
> Default value: _false_

[Special variable]  
__\*erlang-true-is-lisp-t\*__

> Treat the Erlang atom `true` as the Lisp symbol `T` instead of `'|true|`.
> 
> Default value: _false_

[Special variable]  
__\*lisp-nil-at-tail-is-erlang-empty-list\*__

> Treat the Lisp symbol `NIL` at the tail of a list as the empty list instead of
> as the Erlang atom `'NIL'`.
> 
> Default value: _true_

[Special variable]  
__\*lisp-nil-is-erlang-empty-list\*__

> Treat the Lisp symbol `NIL` as the empty list instead of as the Erlang atom
> `'NIL'`.
> 
> Default value: _true_

[Special variable]  
__\*lisp-nil-is-erlang-false\*__

> Treat the Lisp symbol `NIL` as the Erlang atom `false` instead of `'NIL'`.
> 
> Default value: _false_

[Special variable]  
__\*lisp-string-is-erlang-binary\*__

> Treat Lisp strings as Erlang binaries instead of as lists of integers.
> 
> Default value: _false_

[Special variable]  
__\*lisp-t-is-erlang-true\*__

> Treat the Lisp symbol `T` as the Erlang atom `true` instead of `'T'`.
> 
> Default value: _false_


### Types

#### Abstract types

[Type]  
__erlang-translatable__

> The base type for all types of objects that may be translated to Erlang terms.
> 
> Directly translatable types: _integer_, _float_, _symbol_, _string_, and
> _list_ if all elements also are translatable.
> 
> Other types of Erlang terms are translated to subtypes of _erlang-object_.

[Class]  
__erlang-object__

> The base class for all Erlang term types that are not directly translatable to
> Lisp types.

[Class]  
__erlang-fun__ (erlang-object)

> The base class for the different representations of Erlang funs.

[Class]  
__erlang-internal-fun__ (erlang-fun, erlang-object)

> The base class for the two different representations of internal Erlang funs.


#### Concrete types

[Class]  
__erlang-binary__ (erlang-object)

> Represents an Erlang binary or bit-binary.

[Class]  
__erlang-external-fun__ (erlang-fun, erlang-object)

> Represents an external Erlang fun: `fun M:F/A`.

[Class]  
__erlang-new-internal-fun__ (erlang-internal-fun, erlang-fun, erlang-object)

> Represents an internal Erlang fun: `fun F/A` and `fun(Arg1,..) -> ... end`.

[Class]  
__erlang-old-internal-fun__ (erlang-internal-fun, erlang-fun, erlang-object)

> Represents an internal Erlang fun: `fun F/A` and `fun(Arg1,..) -> ... end`.

[Class]  
__erlang-pid__ (erlang-object)

> Represents an Erlang pid.

[Class]  
__erlang-port__ (erlang-object)

> Represents an Erlang port.

[Class]  
__erlang-reference__ (erlang-object)

> Represents an Erlang reference.

[Class]  
__erlang-tuple__ (erlang-object)

> Represents an Erlang tuple.
