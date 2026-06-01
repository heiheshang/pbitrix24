:- module(bitrix24_app, [
             info/2,
             info/3
         ]).

:- use_module(bitrix24_rest).

info(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'app.info', [], Result).

info(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'app.info', [], Result).
