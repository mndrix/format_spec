:- module(format_spec, [ format_spec/2
                       , format_spec//1
                       ]).

:- use_module(library(dcg/basics), [eos//0, integer//1, string_without//2]).
:- use_module(library(error)).
:- use_module(library(when), [when/2]).

% TODO loading this module is optional
% TODO it's for my own convenience during development
:- use_module(library(mavis)).

format_spec([]) -->
    eos.
format_spec([escape(Numeric,Modifier,Action)|Rest]) -->
    "~",
    numeric_argument(Numeric),
    modifier_argument(Modifier),
    action(Action),
    format_spec(Rest).
format_spec([text(String)|Rest]) -->
    { when((ground(String);ground(Codes)),string_codes(String, Codes)) },
    string_without("~", Codes),
    { Codes \= [] },
    format_spec(Rest).


format_spec(Format, Spec) :-
    when((ground(Format);ground(Codes)),text_codes(Format, Codes)),
    phrase(format_spec(Spec), Codes, []).


%% text_codes(Text:text, Codes:codes).
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(String, Codes) :-
    string(String),
    !,
    string_codes(String, Codes).
text_codes(Codes, Codes) :-
    is_of_type(codes, Codes).


numeric_argument(number(N)) -->
    integer(N).
numeric_argument(character(C)) -->
    "`",
    [C].
numeric_argument(star) -->
    "*".
numeric_argument(nothing) -->
    "".


modifier_argument(colon) -->
    ":".
modifier_argument(no_colon) -->
    \+ ":".


action(Action) -->
    [C],
    { is_action(C) },
    { atom_codes(Action, [C]) }.


is_action(0'~).
is_action(0'a).
is_action(0'c).
is_action(0'd).
is_action(0'D).
is_action(0'e).
is_action(0'E).
% TODO ...
is_action(0'n).
% TODO ...
is_action(0's).
% TODO ...
