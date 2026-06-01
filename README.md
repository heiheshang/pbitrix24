
![GitHub](https://img.shields.io/github/license/heiheshang/pbitrix24) ![GitHub](https://img.shields.io/github/last-commit/heiheshang/pbitrix24) ![GitHub](https://img.shields.io/github/v/tag/heiheshang/pbitrix24) ![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-8.x%2B-blue) ![Bitrix24](https://img.shields.io/badge/Bitrix24-REST%20API-2FC6F6)
# About

`bitrix24` is an infrastructure pack for working with the Bitrix24 REST API
from SWI-Prolog.

The core pack now focuses on:

- provider-driven auth and runtime context management,
- bounded OAuth token refresh,
- a shared REST client for Bitrix24 methods,
- install payload normalization into a reusable context shape.

The library no longer treats runtime auth state as built-in storage. A host
application is responsible for implementing the provider contract and deciding
where contexts are stored.

## Install

```prolog
pack_install(bitrix24).
```

## Test

```bash
swipl -g run_tests -t halt prolog/test_bitrix24.plt
```

## Provider Contract

The library expects an application-defined provider module that implements:

```prolog
get_config(Key, Value).
load_context(ContextRef, Context).
save_context(ContextRef, Context).
delete_context(ContextRef).
default_context(ContextRef).
```

Supported context references:

```prolog
global.
member(MemberID).
```

Runtime context is normalized as a dict with `expires_at`:

```prolog
_{
    member_id: MemberID,
    access_token: AccessToken,
    refresh_token: RefreshToken,
    expires_at: ExpiresAt,
    client_endpoint: ClientEndpoint,
    server_endpoint: ServerEndpoint,
    application_token: ApplicationToken,
    user_id: UserID,
    domain: Domain
}
```

See `docs/provider-contract.md` for the full contract.

## Basic REST Usage

```prolog
:- use_module(bitrix24_rest).

check_install_status(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'app.info', [], Result).

call_custom_method(Provider, ContextRef, Method, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, Method, Params, Result).
```

Examples:

```prolog
bitrix24_rest:api_call(MyProvider, member(portal_a), 'user.current', [], Result).
bitrix24_rest:api_call(MyProvider, member(portal_a), 'crm.lead.list',
                       [select=['ID', 'TITLE']], Result).
```

If the provider can resolve a default context, you can use the shorter form:

```prolog
bitrix24_rest:api_call(MyProvider, 'app.info', [], Result).
```

## Install Payload Parsing

Install callbacks can be normalized into `ContextRef + Context` without binding
the library to a particular storage backend:

```prolog
:- use_module(bitrix24_install).

normalize_install_payload(Payload, ContextRef, Context) :-
    bitrix24_install:payload_context(Payload, ContextRef, Context).
```

If you want the library to persist normalized contexts through the provider,
use:

```prolog
persist_install_payload(Provider, Payload, ContextPairs) :-
    bitrix24_install:save_payload_contexts(Provider, Payload, ContextPairs).
```

The helper persists only the normalized `ContextRef` extracted from the payload:
`member(MemberID)` when `auth[member_id]` is present, otherwise `global`.

## Notes

- The core contract is provider-based and storage-agnostic.
- Runtime auth state is no longer persisted by the pack itself.
- Thin wrapper modules remain in the pack, but the core migration is centered on
  `bitrix24_request`, `bitrix24_auth`, `bitrix24_rest`, and `bitrix24_install`.
