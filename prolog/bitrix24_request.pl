:- module(bitrix24_request, [
          post/4, get/3]).

:- autoload(library(option),
            [option/2]).
:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils, [decode_response/2, remove_json/2]).

post(Url, Body, Response, Options) :-
        catch(
         (option(status_code(StatusCode), Options)
          -> http_post(Url, Body, Reply, Options)
          ; http_post(Url, Body, Reply, [status_code(StatusCode) | Options])
         ),
         E,
         throw(error(http_open_error(E),_))
        ),
        status_code(StatusCode, E, Reply, Response, Url).

get(Url, Response, Options) :-
        catch(
         (option(status_code(StatusCode), Options)
           -> http_get(Url, Reply, Options)
          ; http_get(Url, Reply, [status_code(StatusCode) | Options])
         ),
        E,
         throw(error(http_open_error(E),_))
        ),
        status_code(StatusCode, E, Reply, Response, Url).


status_code(200, _E, Reply, Response, _Url) :- !,
    decode_response(Reply, Result),
    remove_json(Result, Response).

status_code(StatusCode, Error, Reply, Response, Url) :-
    decode_response(Reply, Response),
    debug(http(error), 'error status code ~q : error ~q : response ~q : reply ~q url: ~q', [StatusCode, Error, Response, Reply, Url]).
