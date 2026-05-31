:- module(bitrix24_event, [
             get/1,
             bind/2,
             bind/3,
             unbind/2,
             unbind/3
         ]).

:- use_module(bitrix24_rest).

get(Result) :-
    bitrix24_rest:api_call('event.get', [], Result).

bind(Event, Handler) :-
    bind(Event, Handler, []).

bind(Event, Handler, Result) :-
    bitrix24_rest:api_call('event.bind', [event=Event, handler=Handler], Result).

unbind(Event, Handler) :-
    unbind(Event, Handler, []).

unbind(Event, Handler, Result) :-
    bitrix24_rest:api_call('event.unbind', [event=Event, handler=Handler], Result).
