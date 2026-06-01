:- module(bitrix24_provider_memory, [
          reset/0,
          set_config/2,
          set_default_context/1,
          get_config/2,
          load_context/2,
          save_context/2,
          delete_context/1,
          default_context/1
      ]).

:- dynamic stored_config/2.
:- dynamic stored_context/2.
:- dynamic stored_default_context/1.

reset :-
    retractall(stored_config(_, _)),
    retractall(stored_context(_, _)),
    retractall(stored_default_context(_)).

set_config(Key, Value) :-
    retractall(stored_config(Key, _)),
    assertz(stored_config(Key, Value)).

set_default_context(ContextRef) :-
    retractall(stored_default_context(_)),
    assertz(stored_default_context(ContextRef)).

get_config(Key, Value) :-
    stored_config(Key, Value).

load_context(ContextRef, Context) :-
    stored_context(ContextRef, Context).

save_context(ContextRef, Context) :-
    retractall(stored_context(ContextRef, _)),
    assertz(stored_context(ContextRef, Context)).

delete_context(ContextRef) :-
    retractall(stored_context(ContextRef, _)).

default_context(ContextRef) :-
    stored_default_context(ContextRef).
