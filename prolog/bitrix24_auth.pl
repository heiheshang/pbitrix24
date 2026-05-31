:- module(bitrix24_auth, [
             assert_keys/1,
             assert_keys/2,
             check_token/0,
             check_token/1,
             refresh_token/0,
             refresh_token/1
         ]).

:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils).
:- use_module(bitrix24_config).

assert_keys(Data) :-
    assert_keys(global, Data).

assert_keys(global, Data) :-
    !,
    forall(member(Key = Value, Data),
           (
               corresponding(Key, CKey),
               bitrix24_config:retractall_app_info(CKey, _),
               bitrix24_config:db_sync(gc),
               bitrix24_config:assert_app_info(CKey, Value)
           )
        ).
assert_keys(MemberID, Data) :-
    atom(MemberID),
    !,
    assert_keys(member(MemberID), Data).
assert_keys(member(MemberID), Data) :-
    forall(member(Key = Value, Data),
           (
               corresponding(Key, CKey),
               bitrix24_config:retractall_portal_app_info(MemberID, CKey, _),
               bitrix24_config:db_sync(gc),
               bitrix24_config:assert_portal_app_info(MemberID, CKey, Value)
           )
        ).

corresponding('access_token', 'auth[access_token]').
corresponding('refresh_token', 'auth[refresh_token]').
corresponding('server_endpoint', 'auth[server_endpoint]').
corresponding('expires', 'auth[expires]').
corresponding(Key, Key).

check_token :-
    check_token(global).

check_token(global) :-
    !,
    get_time(CurrentTime),
    auth_info(global, 'auth[expires]', ExpiresA),
    (atom(ExpiresA)
        -> atom_number(ExpiresA, Expires)
        ; Expires = ExpiresA
    ),
    Diff is Expires - CurrentTime,
    ( Diff < 100
        -> refresh_token(global)
        ;
           !, true).
check_token(MemberID) :-
    atom(MemberID),
    !,
    check_token(member(MemberID)).
check_token(member(MemberID)) :-
    get_time(CurrentTime),
    auth_info(member(MemberID), 'auth[expires]', ExpiresA),
    ( atom(ExpiresA)
    -> atom_number(ExpiresA, Expires)
    ; Expires = ExpiresA
    ),
    Diff is Expires - CurrentTime,
    ( Diff < 100
    -> refresh_token(member(MemberID))
    ;  !, true
    ).

refresh_token :-
    refresh_token(global).

refresh_token(global) :-
    !,
    auth_info(global, 'auth[refresh_token]', RefreshToken),
    bitrix24_config:config(app, 'application_id', ApplicationId),
    bitrix24_config:config(app, 'client_secret', ClientSecret),
    Attempts = 3,
    refresh_token_attempt(global, Attempts, ApplicationId, ClientSecret, RefreshToken).
refresh_token(MemberID) :-
    atom(MemberID),
    !,
    refresh_token(member(MemberID)).
refresh_token(member(MemberID)) :-
    auth_info(member(MemberID), 'auth[refresh_token]', RefreshToken),
    bitrix24_config:config(app, 'application_id', ApplicationId),
    bitrix24_config:config(app, 'client_secret', ClientSecret),
    Attempts = 3,
    refresh_token_attempt(member(MemberID), Attempts, ApplicationId, ClientSecret, RefreshToken).

refresh_token_attempt(Context, AttemptsLeft, ApplicationId, ClientSecret, RefreshToken) :-
    Url = 'https://oauth.bitrix.info/oauth/token/',
    Body = form([ grant_type=refresh_token,
                  client_id=ApplicationId,
                  client_secret=ClientSecret,
                  refresh_token=RefreshToken
                ]),
    catch(
        http_post(Url, Body, Reply, [status_code(StatusCode)]),
        Error,
        true
    ),
    ( var(Error)
    ->
        refresh_status_code(Context, StatusCode, Reply, AttemptsLeft, ApplicationId, ClientSecret, RefreshToken)
    ;
        refresh_transport_error(Context, AttemptsLeft, Error, ApplicationId, ClientSecret, RefreshToken)
    ).

refresh_transport_error(Context, AttemptsLeft, Error, ApplicationId, ClientSecret, RefreshToken) :-
    ( AttemptsLeft > 1
    ->
        sleep(1),
        NextAttempts is AttemptsLeft - 1,
        refresh_token_attempt(Context, NextAttempts, ApplicationId, ClientSecret, RefreshToken)
    ;
        throw(error(bitrix24_auth_transport_error(Error), _))
    ).

refresh_status_code(Context, 200, Reply, _AttemptsLeft, _ApplicationId, _ClientSecret, _RefreshToken) :-
    bitrix24_utils:decode_response(Reply, Response),
    assert_keys(Context, Response),
    !.
refresh_status_code(Context, StatusCode, Reply, AttemptsLeft, ApplicationId, ClientSecret, RefreshToken) :-
    bitrix24_utils:decode_response(Reply, Response),
    ( AttemptsLeft > 1
    ->
        sleep(1),
        NextAttempts is AttemptsLeft - 1,
        refresh_token_attempt(Context, NextAttempts, ApplicationId, ClientSecret, RefreshToken)
    ;
        throw(error(bitrix24_auth_failed(StatusCode, Response), _))
    ).

auth_info(global, Key, Value) :-
    bitrix24_config:app_info(Key, Value).
auth_info(member(MemberID), Key, Value) :-
    bitrix24_config:portal_app_info(MemberID, Key, Value).
