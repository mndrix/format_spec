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
    once(phrase(format_spec(Spec), Codes, [])).


%% text_codes(Text:text, Codes:codes).
text_codes(Var, Codes) :-
    var(Var),
    !,
    string_codes(Var, Codes).
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


%% is_action(+Action:integer) is semidet.
%% is_action(-Action:integer) is multi.
%
%  True if Action is a valid format/2 action character. Iterates all
%  acceptable action characters, if Action is unbound.
is_action(Action) :-
    action_types(Action, _).

%% action_types(?Action:integer, ?Types:list(type))
%
%  True if Action consumes arguments matching Types. An action (like
%  `~`), which consumes no arguments, has `Types=[]`.  For example,
%
%      ?- action_types(0'~, Types).
%      Types = [].
%      ?- action_types(0'a, Types).
%      Types = [atom].
action_types(0'~, []).
action_types(0'a, [atom]).
action_types(0'c, [integer]).  % specifically, a code
action_types(0'd, [integer]).
action_types(0'D, [integer]).
action_types(0'e, [float]).
action_types(0'E, [float]).
action_types(0'f, [float]).
action_types(0'g, [float]).
action_types(0'G, [float]).
action_types(0'i, [any]).
action_types(0'I, [integer]).
action_types(0'k, [any]).
action_types(0'n, []).
action_types(0'N, []).
action_types(0'p, [any]).
action_types(0'q, [any]).
action_types(0'r, [integer]).
action_types(0'R, [integer]).
action_types(0's, [text]).
action_types(0'@, [callable]).
action_types(0't, []).
action_types(0'|, []).
action_types(0'+, []).
action_types(0'w, [any]).
action_types(0'W, [any, list]).
