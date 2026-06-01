# Provider Contract

`pbitrix24` is provider-driven. The library does not store runtime auth state
internally and instead delegates config and context access to an external
provider module.

## Required callbacks

The consuming application should expose a module that implements:

```prolog
get_config(Key, Value).
load_context(ContextRef, Context).
save_context(ContextRef, Context).
delete_context(ContextRef).
default_context(ContextRef).
```

## Context references

The library works with two context reference forms:

```prolog
global.
member(MemberID).
```

`default_context/1` should resolve the context reference used by
`bitrix24_rest:api_call/4`.

## Runtime context shape

Providers should load and persist runtime context as a dict. The library uses a
single normalized internal shape:

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

## Required context fields

These fields are required for REST calls and token refresh:

- `access_token`
- `refresh_token`
- `expires_at`
- `client_endpoint`

These fields are required when present in install/refresh payloads and should be
preserved by providers:

- `server_endpoint`
- `member_id`
- `user_id`
- `domain`
- `application_token`

## Config keys

Providers must support:

- `client_id`
- `client_secret`

Providers may optionally support:

- `oauth_token_url`

If `oauth_token_url` is not defined, the library defaults to:

```prolog
'https://oauth.bitrix.info/oauth/token/'
```

## Semantics

- `load_context/2` should fail if the requested context does not exist.
- `save_context/2` should replace the stored context for the given reference.
- `delete_context/1` should remove the stored context if it exists.
- `default_context/1` should resolve the context used for implicit API calls.

## Notes

- The library uses `expires_at` internally, not `expires`.
- Install payload parsing is handled by `bitrix24_install:payload_context/3`.
- Install payload persistence can be delegated to
  `bitrix24_install:save_payload_contexts/3`.
- `save_payload_contexts/3` persists only the normalized `ContextRef` extracted
  from the payload.
- OAuth refresh is handled by `bitrix24_auth:refresh_context/3`.
