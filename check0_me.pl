% Load this file then run check/0

:- use_module(library(format_spec)).

main :-
    format("~ oops", [_]),
    format("hi", [superfluous]),
    format(user_error, "~d~n", []),
    debug(something,"Worth $~f", [nothing]).
