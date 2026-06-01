:- module(bitrix24_voximplant, [
             statistic_get/3,
             statistic_get/4,
             line_get/2,
             line_get/3
         ]).

:- use_module(bitrix24_rest).

statistic_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'voximplant.statistic.get', Params, Result).

statistic_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'voximplant.statistic.get', Params, Result).

line_get(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'voximplant.line.get', [], Result).

line_get(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'voximplant.line.get', [], Result).
