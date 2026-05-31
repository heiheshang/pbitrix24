
![GitHub](https://img.shields.io/github/license/heiheshang/pbitrix24) ![GitHub](https://img.shields.io/github/last-commit/heiheshang/pbitrix24) ![GitHub](https://img.shields.io/github/v/tag/heiheshang/pbitrix24) ![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-8.x%2B-blue) ![Bitrix24](https://img.shields.io/badge/Bitrix24-REST%20API-2FC6F6)
# About

`bitrix24` is a collection of tools to make writing prolog programs with bitrix24 api .

The pack now includes:

- persistent config and installation/auth state handling,
- bounded OAuth token refresh,
- a shared REST client for Bitrix24 methods,
- install-time helpers for `event.*` and `placement.*`,
- small wrapper modules for `app.info`, events, placements, users, and CRM lists.

# Quickstart

Run

```prolog
pack_install(bitrix24).
```

Test

``` bash
swipl -g run_tests -t halt prolog/test_bitrix24.plt
```
yaml structure

``` yaml
app:
    application_id: local.63fc74d3bc5678.07353434
    client_secret: DbRl2KqHtSwdF7nJpsTbTpXrIMsdfsdr469LxNzDtzgVFsA2San2hYL
```

``` prolog
[[opt(configdir), type(atom), default('/etc/bitrix24/'), longflags([configdir]),
help('directory for storing settings (for "static" URL)')],
[opt(appdir), type(atom), default('/opt/bitrix24/'), longflags([appdir]),
help('directory for storing settings')]]
```
add to your app
``` prolog
bitrix24_config:load_config(Opts.configdir),
bitrix24_config:catalog_app(Opts.appdir),
bitrix24_config:open_db(Opts.appdir),
```

An example of how to handle application registration
``` prolog
handle_install(Request) :-
    member(method(post), Request),
    member(content_type('application/x-www-form-urlencoded'), Request), !,
    http_read_data(Request, Data, []),
    bitrix24_regapp:install(Data),
    format('Content-type: text/plain~n~n'),
    format('Hello World!~n').
```

## Basic REST usage

After the install callback has stored auth data, you can call Bitrix24 REST
methods through the shared client:

``` prolog
:- use_module(bitrix24_rest).
:- use_module(bitrix24_app).

check_install_status(Result) :-
    bitrix24_app:info(Result).

call_custom_method(Method, Params, Result) :-
    bitrix24_rest:api_call(Method, Params, Result).
```

Examples:

``` prolog
bitrix24_rest:api_call('user.current', [], Result).
bitrix24_rest:api_call('crm.lead.list', [select=['ID', 'TITLE']], Result).
```

## User and CRM helpers

For common domains you can use thin wrappers instead of passing method names
manually:

``` prolog
:- use_module(bitrix24_user).
:- use_module(bitrix24_crm).

current_user(User) :-
    bitrix24_user:current(User).

active_users(Users) :-
    bitrix24_user:list([filter=['ACTIVE'=true], select=['ID', 'NAME']], Users).

open_leads(Leads) :-
    bitrix24_crm:lead_list([filter=['STATUS_ID'='NEW'], select=['ID', 'TITLE']], Leads).
```

For lead sync and CRM helpers:

``` prolog
:- use_module(bitrix24_crm).

create_order_lead(Fields, LeadID) :-
    bitrix24_crm:lead_add([fields=Fields, params=['REGISTER_SONET_EVENT'='Y']], LeadID).

update_order_lead(LeadID, Fields, Result) :-
    bitrix24_crm:lead_update(LeadID, Fields, Result).

attach_lead_contact(LeadID, ContactID, Result) :-
    bitrix24_crm:lead_contact_add(LeadID,
                                  ['CONTACT_ID'=ContactID, 'SORT'=10, 'IS_PRIMARY'='Y'],
                                  Result).
```

For activities, products, and IM profile lookups:

``` prolog
:- use_module(bitrix24_crm).
:- use_module(bitrix24_user).

recent_lead_messages(LeadID, Activities) :-
    bitrix24_crm:activity_list([filter=['OWNER_ID'=LeadID]], Activities).

catalog_matches(Token, Products) :-
    bitrix24_crm:product_list([filter=['%NAME'=Token, 'ACTIVE'='Y'],
                               select=['ID', 'NAME', 'PRICE']],
                              Products).

operator_avatar(UserID, Profile) :-
    bitrix24_user:im_user_get(['ID'=UserID, 'AVATAR_HR'='Y'], Profile).
```

## Open Lines and bot helpers

For connector setup, Open Lines lookups, and notification bots:

``` prolog
:- use_module(bitrix24_imconnector).
:- use_module(bitrix24_imopenlines).
:- use_module(bitrix24_imbot).

register_connector(Result) :-
    bitrix24_imconnector:register(['ID'='aqua_connector',
                                   'NAME'='Aqua Delivery',
                                   'CHAT_GROUP'='N'],
                                  Result).

open_lines(Lines) :-
    bitrix24_imopenlines:config_list_get(['PARAMS'=[select=['ID', 'LINE_NAME', 'ACTIVE']]],
                                         Lines).

send_notification(BotID, DialogID, Result) :-
    bitrix24_imbot:chat_message_send([botId=BotID,
                                      dialogId=DialogID,
                                      fields=[message='New order from Aqua Delivery']],
                                     Result).
```

Typical Bitrix24 scopes for these helpers:

- `crm` for leads, contacts, products, and activities
- `imopenlines` for line listing and session transfer
- `imbot` for bot registration, bot chats, and message/file sending
- `im` for profile-related lookups like `im.user.get`
- `placement` when the app also binds custom placements

## Telephony helpers

For call reports and line metadata:

``` prolog
:- use_module(bitrix24_voximplant).

monthly_calls(PortalNumbers, Calls) :-
    bitrix24_voximplant:statistic_get(['FILTER'=['PORTAL_NUMBER'=PortalNumbers],
                                       'SORT'='CALL_START_DATE',
                                       'ORDER'='ASC'],
                                      Calls).

portal_lines(Lines) :-
    bitrix24_voximplant:line_get(Lines).
```

## Event and placement helpers

The install helper re-registers configured event and placement bindings using
the shared REST client:

``` yaml
event:
  ONCRMLEADADD: https://example.com/bitrix/event

placement:
  LIST_MENU:
    - https://example.com/bitrix/widget
    - CRM Widget
```

You can also call the wrappers directly:

``` prolog
:- use_module(bitrix24_event).
:- use_module(bitrix24_placement).

bitrix24_event:get(Events).
bitrix24_event:bind('ONCRMLEADADD', 'https://example.com/bitrix/event', Reply).
bitrix24_placement:bind('LIST_MENU', 'https://example.com/bitrix/widget', 'CRM Widget', Reply).
```

## Installation notes

- For script-only / API-only applications, Bitrix24 completes installation
  automatically after the install callback is processed.
- For applications with a UI, the frontend installer page must call
  `installFinish`; this is a Bitrix24 frontend step and is not handled by this
  server-side Prolog pack.
- Store `auth[application_token]` from `ONAPPINSTALL`; Bitrix24 documents it as
  important for secure handler flows.
- For multi-portal app integrations keyed by `member_id`, see
  `docs/member-scoped-auth-design.md`.
