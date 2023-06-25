
![GitHub](https://img.shields.io/github/license/heiheshang/pbitrix24)![GitHub](https://img.shields.io/github/last-commit/heiheshang/pbitrix24)![GitHub](https://img.shields.io/github/v/tag/heiheshang/pbitrix24)
# About

`bitrix24` is a collection of tools to make writing prolog programs with bitrix24 api .

# Quickstart

Run

```prolog
pack_install(bitrix24).
```

Test

``` bash
swipl -g run_tests -t halt prolog/test_bitrix24.pl
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
bitrix24_auth:open_db(Opts.appdir),
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
