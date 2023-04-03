:- module(bitrix24_auth, [
             assert_keys/1,
             check_token/0,
             refresh_token/0,
             config/3,
             app_info/2,
             open_db/1
         ]).

 :- dynamic config/3.

:- use_module(library(persistency)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)).
:- use_module(library(http/http_parameters)).

:- persistent
   app_info(key:atom, value:any).

:- setting(database, callable, 'app_info.db',
           "Данные приложения полученные при установке").

assert_keys(Data) :-
    forall(member(Key = Value, Data),
           (
               corresponding(Key, CKey),
               retractall_app_info(CKey, _),
               assert_app_info(CKey, Value)
           )
        ).

corresponding('access_token', 'auth[access_token]').
corresponding('refresh_token', 'auth[refresh_token]').
corresponding('server_endpoint', 'auth[server_endpoint]').
corresponding('expires', 'auth[expires]').
corresponding(Key, Key).

check_token :-
    get_time(CurrentTime),
    app_info('auth[expires]', ExpiresA),
    (atom(ExpiresA)
        -> atom_number(ExpiresA, Expires)
        ; Expires = ExpiresA
    ),
    Diff is Expires - CurrentTime,
    ( Diff < 100
        -> refresh_token
        ;
           true).

refresh_token :-
    app_info('auth[refresh_token]', RefreshToken),
    config(app, 'application_id', ApplicationId),
    config(app, 'client_secret', ClientSecret),
    atomic_list_concat(['https://oauth.bitrix.info/oauth/token/?grant_type=refresh_token',
                        '&client_id=', ApplicationId,
                        '&client_secret=', ClientSecret,
                        '&refresh_token=', RefreshToken], URL),
    repeat,
    catch(
        http_get(URL, Reply, [status_code(StatusCode)]),
        E,
        throw(error(http_open_error(E),_))),
    (200 == StatusCode
        ->
             decode_response(Reply, Response),
             assert_keys(Response),
             true
        ;    decode_response(Reply, Response),
             debug(http(error), 'error ~q : ~q', [E, Response]),
             fail
     ).

open_db(_) :-
    db_attached(_),
    !.
open_db(Dir) :-
    setting(database, Spec),
    atom_concat(Dir, Spec, PathFile),
    db_attach(PathFile, [sync(close)]).

decode_response(json(Reply), Result) :-
    memberchk(result = json(Result), Reply),!.
decode_response(json(Reply), Result) :-
    memberchk(result = Result, Reply),
    is_list(Result),!.
decode_response(json(Reply), Reply).
