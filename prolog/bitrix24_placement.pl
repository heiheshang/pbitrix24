:- module(bitrix24_placement, [
             get/1,
             bind/3,
             bind/4,
             unbind/2,
             unbind/3
         ]).

:- use_module(bitrix24_rest).

get(Result) :-
    bitrix24_rest:api_call('placement.get', [], Result).

bind(Placement, Handler, Title) :-
    bind(Placement, Handler, Title, []).

bind(Placement, Handler, Title, Result) :-
    bitrix24_rest:api_call('placement.bind',
                       ['PLACEMENT'=Placement,
                        'HANDLER'=Handler,
                        'TITLE'=Title],
                       Result).

unbind(Placement, Handler) :-
    unbind(Placement, Handler, []).

unbind(Placement, Handler, Result) :-
    bitrix24_rest:api_call('placement.unbind',
                       ['PLACEMENT'=Placement,
                        'HANDLER'=Handler],
                       Result).
