# Member-Scoped Auth Design

## Goal

Support Bitrix24 app installations for multiple portals and multiple `member_id`
contexts without breaking the current single-installation API.

## Current limitation

The current implementation stores auth data in one global keyspace:

- `bitrix24_config:app_info/2` persists a single `key -> value`
- `bitrix24_auth:check_token/0` reads only global auth keys
- `bitrix24_auth:refresh_token/0` refreshes only the global auth context
- `bitrix24_rest:api_call/4` builds the request only from global auth state

This works for one installed portal, but it cannot safely model the
`robot-bitrix` flow where install state, access token, refresh token, and
`client_endpoint` are scoped by `member_id`.

## Target model

Keep the current global API for backward compatibility and add a second,
member-aware layer on top of the same transport.

### Storage

Add a new persistent store for portal/member auth state:

```prolog
:- persistent
   portal_app_info(member_id:atom, key:atom, value:any).
```

Recommended persisted fields per `member_id`:

- `auth[access_token]`
- `auth[refresh_token]`
- `auth[expires]`
- `auth[client_endpoint]`
- `auth[server_endpoint]`
- `auth[member_id]`
- `auth[user_id]`
- `auth[application_token]`
- optional `public_base_url`

Keep existing `app_info/2` untouched for legacy single-portal usage.

## Proposed API

### Config layer

Add new predicates in `bitrix24_config`:

- `portal_app_info/3`
- `assert_portal_app_info/3`
- `retractall_portal_app_info/3`
- `portal_auth_keys/2`

Example:

```prolog
bitrix24_config:portal_app_info(MemberID, 'auth[access_token]', AccessToken).
```

### Auth layer

Add member-aware auth predicates in `bitrix24_auth`:

- `assert_keys/2`
- `check_token/1`
- `refresh_token/1`

Compatibility rules:

- `assert_keys/1` stays as the legacy global variant
- `check_token/0` stays as the legacy global variant
- `refresh_token/0` stays as the legacy global variant

The new predicates should mirror existing behavior, but operate on
`portal_app_info/3`.

### REST layer

Extend `bitrix24_rest:api_call/4` options to accept a member context:

- `member(MemberID)`

Proposed behavior:

```prolog
bitrix24_rest:api_call(Method, Params, Result, [member(MemberID)]).
```

When `member(MemberID)` is present:

1. call `bitrix24_auth:check_token(MemberID)`
2. read `auth[access_token]` from `portal_app_info/3`
3. read `auth[client_endpoint]` from `portal_app_info/3`
4. on `expired_token`, call `bitrix24_auth:refresh_token(MemberID)`

When the option is absent, keep the current global behavior unchanged.

## Migration strategy

### Step 1

Introduce the new persistent relation and member-aware predicates without
changing existing callers.

### Step 2

Teach install flows to store both:

- legacy global auth via `assert_keys/1`
- member-scoped auth via `assert_keys/2`

This preserves backward compatibility during migration.

### Step 3

Update app-oriented callers to pass `[member(MemberID)]` into
`bitrix24_rest:api_call/4`.

This is the point where `robot-bitrix` style integrations can safely use
multiple portal contexts.

### Step 4

After all portal-aware callers are migrated, optionally reduce dependency on
global auth state for app integrations, while still keeping the global API for
simple script-style pack usage.

## Error semantics

The member-aware flow should preserve current error shapes:

- `bitrix24_api_error(...)`
- `bitrix24_auth_failed(...)`
- `bitrix24_auth_transport_error(...)`
- `bitrix24_http_request_failed(...)`

Only the source of auth state changes; transport and normalization behavior
should remain the same.

## Why this split

This design keeps the current pack useful for simple single-installation cases,
but makes multi-portal app integrations possible without forking the transport
stack or duplicating refresh logic outside `bitrix24_auth` and `bitrix24_rest`.
