:- use_module(library(format_spec)).

:- use_module(library(tap)).

format_spec( "Hello, ~w~n"
           , [ text("Hello, ")
             , escape(nothing, no_colon, w)
             , escape(nothing, no_colon, n)
             ]
           ).

format_spec( '~50f'
           , [ escape(number(50), no_colon, f) ]
           ).

format_spec( "~:d"
           , [ escape(nothing, colon, d) ]
           ).
