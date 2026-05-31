:- module(bitrix24_app, [
             info/1
         ]).

:- use_module(bitrix24_rest).

info(Result) :-
    bitrix24_rest:api_call('app.info', [], Result).
