:- module(bitrix24_user, [
             current/2,
             current/3,
             get/3,
             get/4,
             list/3,
             list/4,
             im_user_get/3,
             im_user_get/4
         ]).

:- use_module(bitrix24_rest).

current(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'user.current', [], Result).

current(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'user.current', [], Result).

get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'user.get', Params, Result).

get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'user.get', Params, Result).

list(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'user.get', Params, Result).

list(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'user.get', Params, Result).

im_user_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'im.user.get', Params, Result).

im_user_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'im.user.get', Params, Result).
