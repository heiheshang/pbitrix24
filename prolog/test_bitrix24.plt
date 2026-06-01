:- module(test_bitrix24, [
          test_bitrix24/0
      ]).

:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(plunit)).
:- use_module(bitrix24_app, []).
:- use_module(bitrix24_auth).
:- use_module(bitrix24_config, []).
:- use_module(bitrix24_event, []).
:- use_module(bitrix24_install).
:- use_module(bitrix24_placement, []).
:- use_module(bitrix24_provider_memory).
:- use_module(bitrix24_request, []).
:- use_module(bitrix24_regapp, []).
:- use_module(bitrix24_rest).
:- use_module(bitrix24_utils).

:- set_prolog_flag(plunit_output, always).

test_bitrix24 :-
    run_tests([
        install,
        auth,
        request,
        rest,
        wrappers,
        regapp,
        decode
    ]).

test_setup :-
    retractall(bitrix24_config:config(_, _, _)),
    bitrix24_provider_memory:reset,
    bitrix24_provider_memory:set_config(client_id, '1234'),
    bitrix24_provider_memory:set_config(client_secret, '123').

test_cleanup :-
    retractall(bitrix24_config:config(_, _, _)),
    bitrix24_provider_memory:reset.

:- begin_tests(install, [setup(test_setup), cleanup(test_cleanup)]).

test(payload_context_from_pairs) :-
    Payload = [
        'auth[access_token]'='install-token',
        'auth[refresh_token]'='install-refresh',
        'auth[expires]'='4102444800',
        'auth[client_endpoint]'='https://portal.example/rest/',
        'auth[server_endpoint]'='https://oauth.bitrix.info/rest/',
        'auth[member_id]'='portal_a',
        'auth[user_id]'='1',
        'auth[domain]'='portal.example',
        'auth[application_token]'='app-token'
    ],
    bitrix24_install:payload_context(Payload, ContextRef, Context),
    assertion(ContextRef == member(portal_a)),
    assertion(Context.access_token == 'install-token'),
    assertion(Context.refresh_token == 'install-refresh'),
    assertion(Context.expires_at == 4102444800),
    assertion(Context.client_endpoint == 'https://portal.example/rest/'),
    assertion(Context.server_endpoint == 'https://oauth.bitrix.info/rest/'),
    assertion(Context.application_token == 'app-token').

test(payload_context_uses_expires_in_when_needed) :-
    get_time(Now),
    Payload = _{
        'auth[access_token]': 'install-token',
        'auth[refresh_token]': 'install-refresh',
        'auth[expires_in]': '3600',
        'auth[client_endpoint]': 'https://portal.example/rest/'
    },
    bitrix24_install:payload_context(Payload, ContextRef, Context),
    assertion(ContextRef == global),
    Delta is Context.expires_at - floor(Now),
    assertion(Delta >= 3598),
    assertion(Delta =< 3602).

test(payload_contexts_include_member_scope_only) :-
    Payload = _{
        'auth[access_token]': 'install-token',
        'auth[refresh_token]': 'install-refresh',
        'auth[expires]': '4102444800',
        'auth[client_endpoint]': 'https://portal.example/rest/',
        'auth[member_id]': 'portal_a'
    },
    bitrix24_install:payload_contexts(Payload, ContextPairs),
    assertion(ContextPairs = [member(portal_a)-_]).

test(save_payload_contexts_persists_selected_scope_only) :-
    Payload = _{
        'auth[access_token]': 'install-token',
        'auth[refresh_token]': 'install-refresh',
        'auth[expires]': '4102444800',
        'auth[client_endpoint]': 'https://portal.example/rest/',
        'auth[member_id]': 'portal_a'
    },
    bitrix24_install:save_payload_contexts(bitrix24_provider_memory, Payload, ContextPairs),
    assertion(ContextPairs = [member(portal_a)-_]),
    bitrix24_provider_memory:load_context(member(portal_a), MemberContext),
    assertion(MemberContext.access_token == 'install-token'),
    assertion(\+ bitrix24_provider_memory:load_context(global, _)),
    assertion(MemberContext.member_id == portal_a).

:- end_tests(install).

:- begin_tests(auth,
               [ setup((test_setup,
                        wrap_predicate(http_client:http_post(Url, Body, Reply, Options), auth_refresh,
                                       _Wrapped, mock_refresh_http_post(Url, Body, Reply, Options)))),
                 cleanup((unwrap_predicate(http_client:http_post(Url, Body, Reply, Options), auth_refresh),
                          test_cleanup)) ]).

test(refresh_context_updates_provider_state) :-
    bitrix24_provider_memory:save_context(member(portal_a), _{
        member_id: portal_a,
        access_token: 'stale-token',
        refresh_token: 'refresh-token',
        expires_at: 1,
        client_endpoint: 'https://portal.example/rest/',
        server_endpoint: 'https://oauth.bitrix.info/rest/',
        domain: 'portal.example'
    }),
    bitrix24_auth:refresh_context(bitrix24_provider_memory, member(portal_a), Context),
    assertion(Context.access_token == new_token),
    assertion(Context.refresh_token == refreshed_token),
    assertion(Context.client_endpoint == 'https://portal.example/rest/'),
    bitrix24_provider_memory:load_context(member(portal_a), Saved),
    assertion(Saved.access_token == new_token).

test(check_context_refreshes_expiring_token) :-
    get_time(Now),
    ExpiringAt is floor(Now) + 10,
    bitrix24_provider_memory:save_context(member(portal_a), _{
        member_id: portal_a,
        access_token: 'old-token',
        refresh_token: 'refresh-token',
        expires_at: ExpiringAt,
        client_endpoint: 'https://portal.example/rest/'
    }),
    bitrix24_auth:check_context(bitrix24_provider_memory, member(portal_a), Context),
    assertion(Context.access_token == new_token).

mock_refresh_http_post('https://oauth.bitrix.info/oauth/token/', form(Form), Reply, [status_code(StatusCode)]) :-
    memberchk(grant_type=refresh_token, Form),
    memberchk(client_id='1234', Form),
    memberchk(client_secret='123', Form),
    memberchk(refresh_token='refresh-token', Form),
    StatusCode = 200,
    Reply = json([access_token=new_token,
                  refresh_token=refreshed_token,
                  expires_in=3600,
                  client_endpoint='https://portal.example/rest/',
                  server_endpoint='https://oauth.bitrix.info/rest/',
                  member_id=portal_a,
                  user_id=1,
                  domain='portal.example']).

:- end_tests(auth).

:- begin_tests(request).

test(post) :-
    request('/post', json(json([id=1])), Reply),
    memberchk('ID'='998145', Reply).

test(network_error_is_structured, [throws(error(bitrix24_http_request_failed(get, 'https://example.com/rest?auth=%5BREDACTED%5D', error(timeout)), _))]) :-
    wrap_predicate(http_client:http_get(Url, Reply, Options), request,
                   _Wrapped, mock_http_get_error(Url, Reply, Options)),
    catch(bitrix24_request:get('https://example.com/rest?auth=123', _Reply, []), Error, (
        unwrap_predicate(http_client:http_get(Url, Reply, Options), request),
        throw(Error)
    )),
    unwrap_predicate(http_client:http_get(Url, Reply, Options), request).

request(Path, Body, Reply) :-
    setup_call_cleanup(
        http_server(http_dispatch, [port(localhost:Port)]),
        ( format(atom(Url), 'http://localhost:~w~w', [Port, Path]),
          bitrix24_request:post(Url, Body, Reply, [])
        ),
        http_stop_server(Port, [])
    ).

mock_http_get_error(_Url, _Reply, _Options) :-
    throw(error(timeout)).

:- http_handler(root(post), post_handler, [methods([post])]).

post_handler(Request) :-
    http_read_json_dict(Request, _Dict),
    Reply = json([result =
                  json(['ID'='998145',
                        'NAME'= @(null),
                        'SOURCE_DESCRIPTION'='Звонок поступил на номер: МТС',
                        'PHONE'=[json(['ID'='1042433',
                                       'VALUE_TYPE'='WORK',
                                       'VALUE'='9143926007',
                                       'TYPE_ID'='PHONE'])]]),
                  time=json([start=1677233302.798346,
                             finish=1677233302.892825])]),
    reply_json(Reply).

:- end_tests(request).

:- begin_tests(rest,
               [ setup((test_setup,
                        bitrix24_provider_memory:save_context(member(portal_a), _{
                            member_id: portal_a,
                            access_token: 'old-token',
                            refresh_token: 'refresh-token',
                            expires_at: 4102444800,
                            client_endpoint: 'https://portal.example/rest/'
                        }),
                        bitrix24_provider_memory:set_default_context(member(portal_a)),
                        nb_setval(rest_calls, []),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), rest_request,
                                       _WrappedRequest, mock_rest_post(Url, Body, Response, Options)),
                        wrap_predicate(http_client:http_post(Url, Body, Reply, Options), rest_refresh,
                                       _WrappedRefresh, mock_rest_refresh_http_post(Url, Body, Reply, Options)))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), rest_request),
                          unwrap_predicate(http_client:http_post(Url, Body, Reply, Options), rest_refresh),
                          test_cleanup)) ]).

test(api_call_uses_default_context) :-
    bitrix24_rest:api_call(bitrix24_provider_memory, 'app.info', [], Reply),
    assertion(memberchk('INSTALLED'=true, Reply)),
    nb_getval(rest_calls, Calls),
    assertion(Calls == ['new-token', 'old-token']).

test(api_call_accepts_explicit_context) :-
    bitrix24_provider_memory:save_context(member(portal_b), _{
        member_id: portal_b,
        access_token: 'other-token',
        refresh_token: 'other-refresh',
        expires_at: 4102444800,
        client_endpoint: 'https://portal-b.example/rest/'
    }),
    bitrix24_rest:api_call(bitrix24_provider_memory, member(portal_b), 'app.info', [], Reply),
    assertion(memberchk('INSTALLED'=true, Reply)).

mock_rest_post('https://portal.example/rest/app.info', json(Body), Response, _Options) :-
    memberchk(auth=Token, Body),
    nb_getval(rest_calls, Calls0),
    nb_setval(rest_calls, [Token|Calls0]),
    ( Token == 'old-token'
    -> Response = [error='expired_token', error_description='token expired']
    ;  Response = ['INSTALLED'=true]
    ).
mock_rest_post('https://portal-b.example/rest/app.info', json(Body), Response, _Options) :-
    memberchk(auth='other-token', Body),
    Response = ['INSTALLED'=true].

mock_rest_refresh_http_post('https://oauth.bitrix.info/oauth/token/', form(Form), Reply, [status_code(StatusCode)]) :-
    memberchk(grant_type=refresh_token, Form),
    memberchk(client_id='1234', Form),
    memberchk(client_secret='123', Form),
    memberchk(refresh_token='refresh-token', Form),
    StatusCode = 200,
    Reply = json([access_token='new-token',
                  refresh_token='new-refresh',
                  expires_in=3600,
                  client_endpoint='https://portal.example/rest/']).

:- end_tests(rest).

:- begin_tests(wrappers,
               [ setup((test_setup,
                        bitrix24_provider_memory:save_context(member(portal_a), _{
                            member_id: portal_a,
                            access_token: 'portal-token',
                            refresh_token: 'portal-refresh',
                            expires_at: 4102444800,
                            client_endpoint: 'https://portal.example/rest/'
                        }),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), wrapper_request,
                                       _Wrapped, mock_wrapper_post(Url, Body, Response, Options)))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), wrapper_request),
                          test_cleanup)) ]).

test(app_info_accepts_explicit_context) :-
    bitrix24_app:info(bitrix24_provider_memory, member(portal_a), Reply),
    assertion(memberchk('INSTALLED'=true, Reply)).

test(event_bind_accepts_explicit_context) :-
    bitrix24_event:bind(bitrix24_provider_memory, member(portal_a),
                        'ONAPPUPDATE', 'https://example.com/hook', Reply),
    assertion(memberchk(result= @(true), Reply)).

test(placement_get_accepts_explicit_context) :-
    bitrix24_placement:get(bitrix24_provider_memory, member(portal_a), Reply),
    assertion(memberchk(placement='CRM_DETAIL_TAB', Reply)).

mock_wrapper_post('https://portal.example/rest/app.info', json(Body), ['INSTALLED'=true], _Options) :-
    memberchk(auth='portal-token', Body).
mock_wrapper_post('https://portal.example/rest/event.bind', json(Body), [result= @(true)], _Options) :-
    memberchk(auth='portal-token', Body),
    memberchk(event='ONAPPUPDATE', Body),
    memberchk(handler='https://example.com/hook', Body).
mock_wrapper_post('https://portal.example/rest/placement.get', json(Body),
                  [placement='CRM_DETAIL_TAB', handler='https://example.com/widget'], _Options) :-
    memberchk(auth='portal-token', Body).

:- end_tests(wrappers).

:- begin_tests(regapp,
               [ setup((test_setup,
                        assertz(bitrix24_config:config(event, 'ONCRMLEADADD', 'https://example.com/event')),
                        assertz(bitrix24_config:config(placement, 'CRM_DETAIL_TAB',
                                                       ['https://example.com/widget', 'Widget'])),
                        nb_setval(regapp_calls, []),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), regapp_request,
                                       _Wrapped, mock_regapp_post(Url, Body, Response, Options)))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), regapp_request),
                          test_cleanup)) ]).

test(install_persists_member_context_and_registers_bindings) :-
    Payload = _{
        'auth[access_token]': 'install-token',
        'auth[refresh_token]': 'install-refresh',
        'auth[expires]': '4102444800',
        'auth[client_endpoint]': 'https://portal.example/rest/',
        'auth[member_id]': 'portal_a'
    },
    bitrix24_regapp:install(bitrix24_provider_memory, Payload, ContextRef),
    assertion(ContextRef == member(portal_a)),
    bitrix24_provider_memory:load_context(member(portal_a), Context),
    assertion(Context.access_token == 'install-token'),
    nb_getval(regapp_calls, Calls),
    assertion(Calls == ['placement.bind', 'placement.unbind', 'placement.get',
                        'event.bind', 'event.unbind', 'event.get']).

mock_regapp_post(Url, json(Body), Response, _Options) :-
    atom_concat('https://portal.example/rest/', Method, Url),
    memberchk(auth='install-token', Body),
    nb_getval(regapp_calls, Calls0),
    nb_setval(regapp_calls, [Method|Calls0]),
    regapp_response(Method, Body, Response).

regapp_response('event.get', _Body,
                [[event='ONAPPINSTALL', handler='https://example.com/install'],
                 [event='ONCRMLEADADD', handler='https://example.com/old-event']]).
regapp_response('event.unbind', Body, [result= @(true)]) :-
    memberchk(event='ONCRMLEADADD', Body),
    memberchk(handler='https://example.com/old-event', Body).
regapp_response('event.bind', Body, [result= @(true)]) :-
    memberchk(event='ONCRMLEADADD', Body),
    memberchk(handler='https://example.com/event', Body).
regapp_response('placement.get', _Body,
                [[placement='CRM_DETAIL_TAB', handler='https://example.com/old-widget']]).
regapp_response('placement.unbind', Body, [count=1]) :-
    memberchk('PLACEMENT'='CRM_DETAIL_TAB', Body),
    memberchk('HANDLER'='https://example.com/old-widget', Body).
regapp_response('placement.bind', Body, [result= @(true)]) :-
    memberchk('PLACEMENT'='CRM_DETAIL_TAB', Body),
    memberchk('HANDLER'='https://example.com/widget', Body),
    memberchk('TITLE'='Widget', Body).

:- end_tests(regapp).

:- begin_tests(decode).

test(decode_response_list) :-
    bitrix24_utils:decode_response(json([result=json([id=7, name='Ada'])]), Result),
    assertion(Result == [id=7, name='Ada']).

test(remove_json) :-
    once(bitrix24_utils:remove_json([json([a=1]), b=2], Result)),
    assertion(Result == [[a=1], b=2]).

:- end_tests(decode).
