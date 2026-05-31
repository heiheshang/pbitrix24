:- module(bitrix24_regapp, [
              install/1
          ]).

:- use_module(bitrix24_auth).
:- use_module(bitrix24_config).
:- use_module(bitrix24_event, []).
:- use_module(bitrix24_placement, []).

install(Data) :-
    bitrix24_auth:assert_keys(Data),
    'event.unbind',
    'event.bind',
    'placement.unbind',
    'placement.bind',
    !.

'event.unbind' :-
    bitrix24_event:get(Reply),
    forall(member(Xs, Reply), (
           memberchk(event=Event, Xs),
           memberchk(handler=Handler, Xs),
           ( Event == 'ONAPPINSTALL'
           -> true
           ;  bitrix24_event:unbind(Event, Handler, UnbindReply),
              successful_unbind(UnbindReply)
           )
       )).

'event.bind' :-
    forall(config(event, E, H), (
               bitrix24_event:bind(E, H, Reply),
               successful_bind(Reply)
           )).

'placement.unbind' :-
    bitrix24_placement:get(Reply),
    forall(member(Xs, Reply), (
           memberchk(placement=Placement, Xs),
           memberchk(handler=Handler, Xs),
           bitrix24_placement:unbind(Placement, Handler, UnbindReply),
           successful_unbind(UnbindReply)
       )).

'placement.bind' :-
    forall(placement_config(E, H, T), (
               bitrix24_placement:bind(E, H, T, Reply),
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
