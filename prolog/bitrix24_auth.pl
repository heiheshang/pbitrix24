:- module(bitrix24_auth, [
             assert_keys/1,
             check_token/0,
             refresh_token/0
         ]).

:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils).
:- use_module(bitrix24_config).

assert_keys(Data) :-
    forall(member(Key = Value, Data),
           (
               corresponding(Key, CKey),
               bitrix24_config:retractall_app_info(CKey, _),
               bitrix24_config:assert_app_info(CKey, Value)
           )
        ).

corresponding('access_token', 'auth[access_token]').
corresponding('refresh_token', 'auth[refresh_token]').
corresponding('server_endpoint', 'auth[server_endpoint]').
corresponding('expires', 'auth[expires]').
corresponding(Key, Key).

check_token :-
    get_time(CurrentTime),
    bitrix24_config:app_info('auth[expires]', ExpiresA),
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
    bitrix24_config:app_info('auth[refresh_token]', RefreshToken),
    bitrix24_config:config(app, 'application_id', ApplicationId),
    bitrix24_config:config(app, 'client_secret', ClientSecret),
    format(atom(Url), '~w~w~w~w~w~w~w', ['https://oauth.bitrix.info/oauth/token/?grant_type=refresh_token',
                        '&client_id=', ApplicationId,
                        '&client_secret=', ClientSecret,
                        '&refresh_token=', RefreshToken]),
    repeat,
    catch(
        http_get(Url, Reply, [status_code(StatusCode)]),
        E,
        throw(error(http_open_error(E),_))),
    (200 == StatusCode
        ->
             bitrix24_utils:decode_response(Reply, Response),
             assert_keys(Response),
             true
        ;    decode_response(Reply, Response),
             debug(http(error), 'error ~q : ~q', [E, Response]),
             fail
     ).
