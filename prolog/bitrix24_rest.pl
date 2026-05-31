:- module(bitrix24_rest, [
             api_call/3,
             api_call/4
         ]).

:- autoload(library(option),
            [option/3]).
:- use_module(bitrix24_auth).
:- use_module(bitrix24_config).
:- use_module(bitrix24_request).

api_call(Method, Params, Result) :-
    api_call(Method, Params, Result, []).

api_call(Method, Params, Result, Options) :-
    option(retries(Retries), Options, 1),
    call_attempt(Method, Params, Result, Retries, Options).

call_attempt(Method, Params, Result, RetriesLeft, Options) :-
    auth_context(Options, Context),
    check_context_token(Context),
    context_auth_info(Context, 'auth[access_token]', AccessToken),
    context_auth_info(Context, 'auth[client_endpoint]', ClientEndpoint),
    endpoint_url(ClientEndpoint, Method, Url),
    Payload = json([auth=AccessToken | Params]),
    bitrix24_request:post(Url, Payload, Response, []),
    normalize_response(Method, Params, Response, Result, RetriesLeft, Options).

normalize_response(Method, Params, Response, Result, RetriesLeft, Options) :-
    bitrix_error(Response, expired_token, _Description),
    RetriesLeft > 0,
    !,
    refresh_context_token(Options),
    NextRetries is RetriesLeft - 1,
    call_attempt(Method, Params, Result, NextRetries, Options).
normalize_response(Method, _Params, Response, _Result, _RetriesLeft, _Options) :-
    bitrix_error(Response, Code, Description),
    !,
    throw(error(bitrix24_api_error(Method, Code, Description, Response), _)).
normalize_response(_Method, _Params, Response, Response, _RetriesLeft, _Options).

bitrix_error(Response, Code, Description) :-
    is_list(Response),
    memberchk(error=Code0, Response),
    normalize_code(Code0, Code),
    ( memberchk(error_description=Description, Response)
    -> true
    ; Description = ''
    ).

normalize_code(Code, Normalized) :-
    atom(Code),
    !,
    downcase_atom(Code, Normalized).
normalize_code(Code, Normalized) :-
    string(Code),
    !,
    string_lower(Code, Lower),
    atom_string(Normalized, Lower).
normalize_code(Code, Code).

endpoint_url(ClientEndpoint, Method, Url) :-
    ( sub_atom(ClientEndpoint, _, 1, 0, '/')
    -> atom_concat(ClientEndpoint, Method, Url)
    ;  format(atom(Url), '~w/~w', [ClientEndpoint, Method])
    ).

auth_context(Options, member(MemberID)) :-
    option(member(MemberID), Options),
    !.
auth_context(_Options, global).

check_context_token(global) :-
    bitrix24_auth:check_token.
check_context_token(member(MemberID)) :-
    bitrix24_auth:check_token(MemberID).

refresh_context_token(Options) :-
    auth_context(Options, global),
    !,
    bitrix24_auth:refresh_token.
refresh_context_token(Options) :-
    auth_context(Options, member(MemberID)),
    bitrix24_auth:refresh_token(MemberID).

context_auth_info(global, Key, Value) :-
    bitrix24_config:app_info(Key, Value).
context_auth_info(member(MemberID), Key, Value) :-
    bitrix24_config:portal_app_info(MemberID, Key, Value).
