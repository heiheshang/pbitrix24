:- module(bitrix24_user, [
             current/1,
             get/2,
             list/2,
             im_user_get/2
         ]).

:- use_module(bitrix24_rest).

current(Result) :-
    bitrix24_rest:api_call('user.current', [], Result).

get(Params, Result) :-
    bitrix24_rest:api_call('user.get', Params, Result).

list(Params, Result) :-
    bitrix24_rest:api_call('user.get', Params, Result).

im_user_get(Params, Result) :-
    bitrix24_rest:api_call('im.user.get', Params, Result).
