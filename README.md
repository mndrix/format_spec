# Synopsis

    ?- use_module(library(format_spec)).
    ?- format_spec("Hello, ~s~n", Spec).
    Spec = [ text("Hello, ")
           , escape(nothing, no_colon, s)
           , escape(nothing, no_colon, n)
           ].

    ?- spec_arity($Spec, Arity).
    Arity = 1.

    ?- spec_types($Spec, Types).
    Types = [text].

    ?- check.
    Warning: foo.pl:6:4:
        In goal: format("~ oops",[_G3495])
        Invalid format string: "~ oops"
    Warning: foo.pl:7:4:
        In goal: format("hi",[superfluous])
        Wrong argument count. Expected 0, got 1
    Warning: foo.pl:9:4:
        In goal: debug(something,"Worth $~f",[nothing])
        Type error: `float' expected, found `nothing' (an atom)

# Description

Parse, analyze and check your format/2 strings.  In SWI Prolog 7.1.5 or later, load this module and run `check/0` to check your code for common format string errors.  If you only want the `check/0` integration, load the module with

    :- use_module(library(format_spec), [])).

See Synopsis above for examples.

# Changes in this Version

  * Initial public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(format_spec).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/format_spec

@author Michael Hendricks <michael@ndrix.org>
@license BSD
