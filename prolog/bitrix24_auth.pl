:- module(bitrix24_auth, [
          load_context/3,
          check_context/3,
          refresh_context/3,
          context_expiring_soon/1
      ]).

:- use_module(library(http/http_client)).
:- use_module(bitrix24_utils).

load_context(Provider, ContextRef, Context) :-
    call(Provider:load_context(ContextRef, Context)).

check_context(Provider, ContextRef, Context) :-
    load_context(Provider, ContextRef, Context0),
    (   context_expiring_soon(Context0)
    ->  refresh_context(Provider, ContextRef, Context)
    ;   Context = Context0
    ).

context_expiring_soon(Context) :-
    get_dict(expires_at, Context, ExpiresAt0),
    normalize_integer(ExpiresAt0, ExpiresAt),
    get_time(Now),
    Diff is ExpiresAt - Now,
    Diff < 100.

refresh_context(Provider, ContextRef, Context) :-
    load_context(Provider, ContextRef, Context0),
    call(Provider:get_config(client_id, ClientID)),
    call(Provider:get_config(client_secret, ClientSecret)),
    oauth_token_url(Provider, Url),
    RefreshToken = Context0.refresh_token,
    Attempts = 3,
    refresh_attempt(Attempts, Url, ClientID, ClientSecret, RefreshToken, Context0, Context),
    call(Provider:save_context(ContextRef, Context)).

oauth_token_url(Provider, Url) :-
    (   call(Provider:get_config(oauth_token_url, Url0))
    ->  Url = Url0
    ;   Url = 'https://oauth.bitrix.info/oauth/token/'
    ).

refresh_attempt(AttemptsLeft, Url, ClientID, ClientSecret, RefreshToken, Context0, Context) :-
    Body = form([ grant_type=refresh_token,
                  client_id=ClientID,
                  client_secret=ClientSecret,
                  refresh_token=RefreshToken
                ]),
    catch(
        http_post(Url, Body, Reply, [status_code(StatusCode)]),
        Error,
        true
    ),
    (   var(Error)
    ->  handle_refresh_reply(StatusCode, Reply, Context0, Context, AttemptsLeft,
                             Url, ClientID, ClientSecret, RefreshToken)
    ;   handle_refresh_transport_error(Error, AttemptsLeft, Url, ClientID, ClientSecret,
                                       RefreshToken, Context0, Context)
    ).

handle_refresh_transport_error(Error, AttemptsLeft, Url, ClientID, ClientSecret, RefreshToken,
                               Context0, Context) :-
    (   AttemptsLeft > 1
    ->  sleep(1),
        NextAttempts is AttemptsLeft - 1,
        refresh_attempt(NextAttempts, Url, ClientID, ClientSecret, RefreshToken, Context0, Context)
    ;   throw(error(bitrix24_auth_transport_error(Error), _))
    ).

handle_refresh_reply(200, Reply, Context0, Context, _AttemptsLeft, _Url, _ClientID,
                     _ClientSecret, _RefreshToken) :-
    !,
    bitrix24_utils:decode_response(Reply, Response),
    merge_refresh_response(Context0, Response, Context).
handle_refresh_reply(StatusCode, Reply, Context0, Context, AttemptsLeft, Url, ClientID,
                     ClientSecret, RefreshToken) :-
    bitrix24_utils:decode_response(Reply, Response),
    (   AttemptsLeft > 1
    ->  sleep(1),
        NextAttempts is AttemptsLeft - 1,
        refresh_attempt(NextAttempts, Url, ClientID, ClientSecret, RefreshToken, Context0, Context)
    ;   throw(error(bitrix24_auth_failed(StatusCode, Response), _))
    ).

merge_refresh_response(Context0, Response, Context) :-
    response_field(Response, access_token, AccessToken),
    response_field(Response, refresh_token, RefreshToken),
    response_field(Response, expires_in, ExpiresIn0),
    normalize_integer(ExpiresIn0, ExpiresIn),
    get_time(Now),
    ExpiresAt is floor(Now) + ExpiresIn - 60,
    Context1 = Context0.put(_{
        access_token: AccessToken,
        refresh_token: RefreshToken,
        expires_at: ExpiresAt
    }),
    maybe_put_field(Context1, Response, client_endpoint, Context2),
    maybe_put_field(Context2, Response, server_endpoint, Context3),
    maybe_put_field(Context3, Response, domain, Context4),
    maybe_put_field(Context4, Response, member_id, Context5),
    maybe_put_field(Context5, Response, user_id, Context).

maybe_put_field(Context0, Response, Field, Context) :-
    (   response_optional_field(Response, Field, Value)
    ->  Context = Context0.put(Field, Value)
    ;   Context = Context0
    ).

response_field(Response, Field, Value) :-
    is_dict(Response),
    !,
    get_dict(Field, Response, Value).
response_field(Response, Field, Value) :-
    memberchk(Field=Value, Response).

response_optional_field(Response, Field, Value) :-
    is_dict(Response),
    !,
    get_dict(Field, Response, Value).
response_optional_field(Response, Field, Value) :-
    memberchk(Field=Value, Response).

normalize_integer(Value, Integer) :-
    integer(Value),
    !,
    Integer = Value.
normalize_integer(Value, Integer) :-
    number(Value),
    !,
    Integer is floor(Value).
normalize_integer(Value, Integer) :-
    string(Value),
    !,
    number_string(Integer, Value).
normalize_integer(Value, Integer) :-
    atom(Value),
    atom_number(Value, Integer).
