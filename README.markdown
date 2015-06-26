Automatic Testing of Curry Programs
===================================

This repository provides the implementations of automatic test tools
described in my dissertation [On Functional Logic Programming and its
Application to Testing][diss]. Currently, my dissertation is the only
available documentation for these programs but here is a short
description of the files in this repository.

[diss]: http://www-ps.informatik.uni-kiel.de/~sebf/thesis.pdf

`BlackCheck.curry` implements several functions for black-box testing
and combinators to construct properties of possibly nondeterministic
operations. This tool is a reimplementation of the ideas previously
implemented in [EasyCheck][EasyCheck] extended with different search
strategies.

[EasyCheck]: http://www-ps.informatik.uni-kiel.de/~sebf/data/pub/flops08.pdf

The provided test functions search for test input using the following
strategies:

  * `blackCheck` uses level diagonalisation like `diagCheck` which
    allows to tweak certain parameters,

  * `quickCheck` uses random search like `rndCheck` which has more
    parameters for tweaking the search characteristics,

  * `smallCheck` performs iterative deepening depth-first search,
    `depthCheck` enumerates the same tests but not iteratively, and
    `iterCheck` is the generalization of both with more parameters.

`GlassCheck.curry` implements several functions for glass-box testing.

  * `quickCheck` and `rndCheck` again implement random testing,

  * `smallCheck` and `depthCheck` again search up to certain level
    either iteratively or not, and

  * `sparseCheck` is a randomized version of `discrCheck`. Both
    perform limited discrepancy search, a variant of level
    diagonalization.

Glass-box testing is superior for strong preconditions, so I recommend
using `smallCheck` and `depthCheck` in `GlassCheck.curry` for testing
deterministic functions. If errors are only exposed by big test input
or you want to test a nondeterministic operation, I recommend using
black-box testing with random search.

The file `blackbox.curry` contains some examples of black-box testing
from my dissertation, some of which use heaps defined in
`Heap.curry`. Some heap properties use lazy predicates implemented in
`Answer.curry`. The type class `Arbitrary` defined in
`Arbitrary.curry` is used to specify test-case generators for both
black-box and glass-box testing. The remaining files
`SearchStrategies.curry` and `TreeSearch.curry` are used internally to
implement glass-box testing.

Because of the `Arbitrary` class used for specifying test-case
generators, these modules require the type-class branch of the
[Münster Curry Compiler][MCC].

[MCC]: http://danae.uni-muenster.de/~lux/curry/

Alle Quelltexte von `curry-test` sind unter der [CC BY-SA 4.0] Lizenz veröffentlicht.

[CC By-SA 4.0]: https://creativecommons.org/licenses/by-sa/4.0/
