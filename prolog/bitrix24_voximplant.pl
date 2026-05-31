:- module(bitrix24_voximplant, [
             statistic_get/2,
             line_get/1
         ]).

:- use_module(bitrix24_rest).

statistic_get(Params, Result) :-
    bitrix24_rest:api_call('voximplant.statistic.get', Params, Result).

line_get(Result) :-
    bitrix24_rest:api_call('voximplant.line.get', [], Result).
