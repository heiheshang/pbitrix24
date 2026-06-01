:- module(bitrix24_regapp, [
              install/2,
              install/3
          ]).

:- use_module(bitrix24_config).
:- use_module(bitrix24_install).
:- use_module(bitrix24_event, []).
:- use_module(bitrix24_placement, []).

install(Provider, Data) :-
    install(Provider, Data, _).

install(Provider, Data, ContextRef) :-
    bitrix24_install:save_payload_contexts(Provider, Data, [ContextRef-_]),
    'event.unbind'(Provider, ContextRef),
    'event.bind'(Provider, ContextRef),
    'placement.unbind'(Provider, ContextRef),
    'placement.bind'(Provider, ContextRef),
    !.

'event.unbind'(Provider, ContextRef) :-
    bitrix24_event:get(Provider, ContextRef, Reply),
    forall(member(Xs, Reply), (
           memberchk(event=Event, Xs),
           memberchk(handler=Handler, Xs),
           ( Event == 'ONAPPINSTALL'
           -> true
           ;  bitrix24_event:unbind(Provider, ContextRef, Event, Handler, UnbindReply),
              successful_unbind(UnbindReply)
           )
       )).

'event.bind'(Provider, ContextRef) :-
    forall(config(event, E, H), (
               bitrix24_event:bind(Provider, ContextRef, E, H, Reply),
               successful_bind(Reply)
           )).

'placement.unbind'(Provider, ContextRef) :-
    bitrix24_placement:get(Provider, ContextRef, Reply),
    forall(member(Xs, Reply), (
           memberchk(placement=Placement, Xs),
           memberchk(handler=Handler, Xs),
           bitrix24_placement:unbind(Provider, ContextRef, Placement, Handler, UnbindReply),
           successful_unbind(UnbindReply)
       )).

'placement.bind'(Provider, ContextRef) :-
    forall(placement_config(E, H, T), (
               bitrix24_placement:bind(Provider, ContextRef, E, H, T, Reply),
               successful_bind(Reply)
           )).

placement_config(Placement, Handler, Title) :-
    config(placement, Placement, [Handler, Title]),
    !.
placement_config(Placement, Handler, Title) :-
    config(placement, Placement, Handler),
    Title = Handler.

successful_bind(Reply) :-
    memberchk(result = @(true), Reply),
    !.
successful_bind(Reply) :-
    throw(error(bitrix24_registration_failed(Reply), _)).

successful_unbind(Reply) :-
    ( memberchk(result = @(true), Reply)
    ; memberchk(count = _, Reply)
    ),
    !.
successful_unbind(Reply) :-
    throw(error(bitrix24_registration_failed(Reply), _)).
