:- module(bitrix24_request, [
          post/4, get/3]).

:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils, [decode_response/2, remove_json/2]).

post(URL, Body, Options, Response) :-
    catch(
        http_post(URL, Body, Reply, [status_code(StatusCode) | Options]),
        E,
        throw(error(http_open_error(E),_))
    ),
    (200 == StatusCode
        ->
             decode_response(Reply, Result),
             remove_json(Result, Response)
        ;    decode_response(Reply, Response),
             debug(http(error), 'error ~q : ~q', [E, Response])
     ).

get(Url, Response, Options) :-
    http_get(Url, Reply, Options),
    decode_response(Reply, Response).
