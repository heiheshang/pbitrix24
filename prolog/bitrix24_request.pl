:- module(bitrix24_request, [
          post/4, get/3]).

:- autoload(library(option),
            [option/2]).
:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils, [decode_response/2, remove_json/2]).

post(Url, Body, Response, Options) :-
        (option(status_code(StatusCode), Options)
         -> http_post(Url, Body, Reply, Options)
        ; http_post(Url, Body, Reply, [status_code(StatusCode) | Options])
        ),
        status_code(StatusCode, Reply, Response).

get(Url, Response, Options) :-
        (option(status_code(StatusCode), Options)
          -> http_get(Url, Reply, Options)
         ; http_get(Url, Reply, [status_code(StatusCode) | Options])
        ),
        status_code(StatusCode, Reply, Response).


status_code(200, Reply, Response) :- !,
    decode_response(Reply, Result),
    remove_json(Result, Response).

status_code(StatusCode, Reply, Response) :-
    decode_response(Reply, Response),
    debug(http(error), 'error status code ~q : reply ~q', [StatusCode, Reply]).
