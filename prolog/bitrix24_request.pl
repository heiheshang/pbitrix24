:- module(bitrix24_request, [
          post/4, get/3]).

:- autoload(library(option),
            [option/2]).
:- use_module(library(http/http_client)).
:- use_module(library(uri)).
:- use_module(bitrix24_utils, [decode_response/2, remove_json/2]).

:- debug(request(post)).
:- debug(request(get)).
post(Url, Body, Response, Options) :-
    http_request(post, Url, Body, Response, Options).

get(Url, Response, Options) :-
    http_request(get, Url, _, Response, Options).

http_request(post, Url, Body, Response, Options) :-
    execute_request(post, Url, Body, Reply, StatusCode, Options),
    status_code(StatusCode, Reply, Response, Url).
http_request(get, Url, Body, Response, Options) :-
    execute_request(get, Url, Body, Reply, StatusCode, Options),
    status_code(StatusCode, Reply, Response, Url).

execute_request(post, Url, Body, Reply, StatusCode, Options) :-
    request_options(Options, StatusCode, HttpOptions),
    catch(
        http_post(Url, Body, Reply, HttpOptions),
        Error,
        throw_request_error(post, Url, Error)
    ).
execute_request(get, Url, _Body, Reply, StatusCode, Options) :-
    request_options(Options, StatusCode, HttpOptions),
    catch(
        http_get(Url, Reply, HttpOptions),
        Error,
        throw_request_error(get, Url, Error)
    ).

request_options(Options, _StatusCode, Options) :-
    option(status_code(_), Options),
    !.
request_options(Options, StatusCode, [status_code(StatusCode)|Options]).

throw_request_error(Method, Url, Error) :-
    redact_url(Url, SafeUrl),
    debug(request(Method), '~q~n', [Error]),
    throw(error(bitrix24_http_request_failed(Method, SafeUrl, Error), _)).

status_code(200, Reply, Response, _Url) :- !,
    decode_response(Reply, Result),
    remove_json(Result, Response).

status_code(StatusCode, Reply, Response, Url) :-
    decode_response(Reply, Response),
    redact_url(Url, SafeUrl),
    debug(http(error), 'error status code ~q : response ~q : reply ~q url: ~q', [StatusCode, Response, Reply, SafeUrl]).

redact_url(Url, SafeUrl) :-
    uri_components(Url, Components),
    uri_data(search, Components, Search),
    nonvar(Search),
    Search \== '',
    !,
    uri_query_components(Search, QueryComponents),
    maplist(redact_query_component, QueryComponents, SafeComponents),
    uri_query_components(SafeSearch, SafeComponents),
    uri_data(search, Components, SafeSearch, SafeComponentsRecord),
    uri_components(SafeUrl, SafeComponentsRecord).
redact_url(Url, Url).

redact_query_component(Name=_, Name='[REDACTED]') :-
    secret_query_key(Name),
    !.
redact_query_component(Component, Component).

secret_query_key(auth).
secret_query_key(client_secret).
secret_query_key(refresh_token).
secret_query_key(access_token).
