:- module(bitrix24_rest, [
          api_call/4,
          api_call/5
      ]).

:- use_module(bitrix24_auth).
:- use_module(bitrix24_request).

api_call(Provider, Method, Params, Result) :-
    call(Provider:default_context(ContextRef)),
    api_call(Provider, ContextRef, Method, Params, Result).

api_call(Provider, ContextRef, Method, Params, Result) :-
    api_call_once(Provider, ContextRef, Method, Params, Result, false).

api_call_once(Provider, ContextRef, Method, Params, Result, Retried) :-
    bitrix24_auth:check_context(Provider, ContextRef, Context),
    endpoint_url(Context.client_endpoint, Method, Url),
    Payload = json([auth=Context.access_token | Params]),
    bitrix24_request:post(Url, Payload, Response, []),
    normalize_response(Provider, ContextRef, Method, Params, Response, Result, Retried).

normalize_response(Provider, ContextRef, Method, Params, Response, Result, false) :-
    bitrix_error(Response, expired_token, _Description),
    !,
    bitrix24_auth:refresh_context(Provider, ContextRef, _),
    api_call_once(Provider, ContextRef, Method, Params, Result, true).
normalize_response(_Provider, _ContextRef, Method, _Params, Response, _Result, _Retried) :-
    bitrix_error(Response, Code, Description),
    !,
    throw(error(bitrix24_api_error(Method, Code, Description, Response), _)).
normalize_response(_Provider, _ContextRef, _Method, _Params, Response, Response, _Retried).

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
