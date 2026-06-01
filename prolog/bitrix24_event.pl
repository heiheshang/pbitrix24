:- module(bitrix24_event, [
             get/2,
             get/3,
             bind/3,
             bind/4,
             bind/5,
             unbind/3,
             unbind/4,
             unbind/5
         ]).

:- use_module(bitrix24_rest).

get(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'event.get', [], Result).

get(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'event.get', [], Result).

bind(Provider, Event, Handler) :-
    bind(Provider, Event, Handler, []).

bind(Provider, Event, Handler, Result) :-
    bitrix24_rest:api_call(Provider, 'event.bind', [event=Event, handler=Handler], Result).

bind(Provider, ContextRef, Event, Handler, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'event.bind',
                           [event=Event, handler=Handler], Result).

unbind(Provider, Event, Handler) :-
    unbind(Provider, Event, Handler, []).

unbind(Provider, Event, Handler, Result) :-
    bitrix24_rest:api_call(Provider, 'event.unbind', [event=Event, handler=Handler], Result).

unbind(Provider, ContextRef, Event, Handler, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'event.unbind',
                           [event=Event, handler=Handler], Result).
