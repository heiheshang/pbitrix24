:- module(bitrix24_placement, [
             get/2,
             get/3,
             bind/4,
             bind/5,
             bind/6,
             unbind/3,
             unbind/4,
             unbind/5
         ]).

:- use_module(bitrix24_rest).

get(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'placement.get', [], Result).

get(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'placement.get', [], Result).

bind(Provider, Placement, Handler, Title) :-
    bind(Provider, Placement, Handler, Title, []).

bind(Provider, Placement, Handler, Title, Result) :-
    bitrix24_rest:api_call(Provider, 'placement.bind',
                           ['PLACEMENT'=Placement,
                            'HANDLER'=Handler,
                            'TITLE'=Title],
                           Result).

bind(Provider, ContextRef, Placement, Handler, Title, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'placement.bind',
                           ['PLACEMENT'=Placement,
                            'HANDLER'=Handler,
                            'TITLE'=Title],
                           Result).

unbind(Provider, Placement, Handler) :-
    unbind(Provider, Placement, Handler, []).

unbind(Provider, Placement, Handler, Result) :-
    bitrix24_rest:api_call(Provider, 'placement.unbind',
                           ['PLACEMENT'=Placement,
                            'HANDLER'=Handler],
                           Result).

unbind(Provider, ContextRef, Placement, Handler, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'placement.unbind',
                           ['PLACEMENT'=Placement,
                            'HANDLER'=Handler],
                           Result).
