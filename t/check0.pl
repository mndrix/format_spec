:- use_module(library(format_spec)).

:- use_module(library(tap)).

format_error( format("~ oops", [_])
            , "Invalid format string: \"~ oops\""
            ).

format_error( format("hi", [superfluous])
            , "Wrong argument count. Expected 0, got 1"
            ).

format_error( format(user_error, "~d~n", [])
            , "Wrong argument count. Expected 1, got 0"
            ).

'arguments of the incorrect type' :-
    findall( Error
           , format_error(format("~s: ~d~n", [foo, bar]), Error)
           , Errors
           ),
    Errors == ["Type error: `integer' expected, found `bar' (an atom)"].

'no errors when argument list is a variable' :-
    \+ format_error(format("stuff: ~w", _), _).

'no type errors when argument is a variable' :-
    \+ format_error(format(atom(_),"~f",[_]),_).
