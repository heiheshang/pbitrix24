:- module(test_bitrix24, [
              test_bitrix24/0
          ]).

:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(plunit)).
:- use_module(bitrix24_app).
:- use_module(bitrix24_auth).
:- use_module(bitrix24_config).
:- use_module(bitrix24_crm).
:- use_module(bitrix24_imbot).
:- use_module(bitrix24_imconnector).
:- use_module(bitrix24_imopenlines).
:- use_module(bitrix24_regapp).
:- use_module(bitrix24_request).
:- use_module(bitrix24_rest).
:- use_module(bitrix24_user).
:- use_module(bitrix24_utils).
:- use_module(bitrix24_voximplant).

:- dynamic temp_test_dir/1.

:- debug(log).
:- set_prolog_flag(plunit_output, always).

test_bitrix24 :-
    run_tests([
               assert_keys,
               load_config,
               refresh,
               member_auth,
               request,
               rest,
               install,
               domain_api,
               decode
              ]).

test_setup :-
    retractall(bitrix24_config:config(_, _, _)),
    retractall(bitrix24_config:app_info(_, _)),
    make_temp_dir(Dir),
    asserta(temp_test_dir(Dir)),
    bitrix24_config:open_db(Dir).

test_cleanup :-
    retractall(bitrix24_config:config(_, _, _)),
    retractall(bitrix24_config:app_info(_, _)),
    retractall(bitrix24_config:portal_app_info(_, _, _)),
    bitrix24_config:db_sync(gc),
    retractall(temp_test_dir(_)).

make_temp_dir(Dir) :-
    tmp_file(bitrix24_tests_, TmpBase),
    make_directory(TmpBase),
    atom_concat(TmpBase, '/', Dir).

temp_path(FileName, Path) :-
    temp_test_dir(Dir),
    atom_concat(Dir, FileName, Path).

:- begin_tests(assert_keys, [setup(test_setup), cleanup(test_cleanup)]).

test(assert_keys) :-
    Data = ['data[VERSION]'='1','data[ACTIVE]'='Y','data[INSTALLED]'='Y','data[LANGUAGE_ID]'=ru,ts='1677593645','auth[access_token]'='3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6','auth[expires]'='1677597245','auth[expires_in]'='3600','auth[scope]'=crm,'auth[domain]'='test.bitrix24.ru','auth[server_endpoint]'='https://oauth.bitrix.info/rest/','auth[status]'='L','auth[client_endpoint]'='https://test.bitrix24.ru/rest/','auth[member_id]'=bf778c7d11d1fd067221619b61deb0c0,'auth[user_id]'='1','auth[refresh_token]'='2d9925640061aca0002c4d4000000001000007a98d6991b6116d0be1c4982a6a535a38','auth[application_token]'='470fca5b93a683685254564bf32eafbcaf'],
    bitrix24_auth:assert_keys(Data),
    bitrix24_config:app_info('auth[access_token]', '3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6'),
    Data1 = ['key1'=['value1', 'value2']],
    bitrix24_auth:assert_keys(Data1),
    bitrix24_config:app_info('key1', ['value1', 'value2']).

:- end_tests(assert_keys).

:- begin_tests(load_config, [setup(test_setup), cleanup(test_cleanup)]).

test(load_yaml_config) :-
    temp_path('app.yaml', ConfigPath),
    setup_call_cleanup(
        open(ConfigPath, write, Stream),
        format(Stream,
               'app:~n  application_id: local.test~n  client_secret: supersecret~nevent:~n  onCrmAdd: https://example.com/hook~nplacement:~n  LIST_MENU:~n    - https://example.com/widget~n    - Menu title~n',
               []),
        close(Stream)
    ),
    temp_test_dir(Dir),
    bitrix24_config:load_config(Dir),
    assertion(bitrix24_config:config(app, 'application_id', 'local.test')),
    assertion(bitrix24_config:config(app, 'client_secret', 'supersecret')),
    assertion(bitrix24_config:config(event, 'onCrmAdd', 'https://example.com/hook')),
    assertion(bitrix24_config:config(placement, 'LIST_MENU', ['https://example.com/widget', 'Menu title'])).

:- end_tests(load_config).

:- begin_tests(refresh,
               [ setup((test_setup,
                        assertz(bitrix24_config:config(app, 'application_id', '1234')),
                        assertz(bitrix24_config:config(app, 'client_secret', '123')),
                        nb_setval(refresh_attempts, 0),
                        nb_setval(refresh_mode, success),
                        wrap_predicate(http_client:http_post(Url, Body, Reply, Options), refresh,
                                       _Wrapped, mock_refresh_http_post(Url, Body, Reply, Options)))),
                 cleanup((unwrap_predicate(http_client:http_post(Url, Body, Reply, Options), refresh),
                          test_cleanup)) ]).

test(refresh) :-
    Data = [access_token='69b4fe630061aca0002c4d4000003681000007fd7ac2b201ddab0a6243287a5c561a50',expires=1677636713,expires_in=3600,scope=app,domain='oauth.bitrix.info',server_endpoint='https://oauth.bitrix.info/rest/',status='L',client_endpoint='https://test.bitrix24.ru/rest/',member_id=bf778c7d11d1fd065547919b61deb0c0,user_id=1,refresh_token='593326640061aca0002c4d40048600015670007e15f25b78deb2ce06bd959e16225c15d'],
    bitrix24_auth:assert_keys(Data),
    bitrix24_auth:refresh_token,
    assertion(bitrix24_config:app_info('auth[access_token]', c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0)).

test(refresh_bounded_retry, [throws(error(bitrix24_auth_failed(401, [error=invalid_grant,error_description='bad refresh token']), _))]) :-
    nb_setval(refresh_attempts, 0),
    nb_setval(refresh_mode, failure),
    Data = [access_token='stale',expires=1677636713,refresh_token='bad'],
    bitrix24_auth:assert_keys(Data),
    catch(bitrix24_auth:refresh_token, Error, (
        nb_getval(refresh_attempts, Attempts),
        assertion(Attempts == 3),
        throw(Error)
    )).

mock_refresh_http_post('https://oauth.bitrix.info/oauth/token/', form(Form), Reply, [status_code(StatusCode)]) :-
    nb_getval(refresh_attempts, Attempts0),
    Attempts is Attempts0 + 1,
    nb_setval(refresh_attempts, Attempts),
    memberchk(grant_type=refresh_token, Form),
    memberchk(client_id='1234', Form),
    memberchk(client_secret='123', Form),
    memberchk(refresh_token=_, Form),
    nb_getval(refresh_mode, Mode),
    mock_refresh_response(Mode, StatusCode, Reply).

mock_refresh_response(success, 200,
                      json([access_token=c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0,
                            expires=1680418752,
                            expires_in=3600,
                            scope=app,
                            domain='oauth.bitrix.info',
                            server_endpoint='https://oauth.bitrix.info/rest/',
                            status='L',
                            client_endpoint='http://localhost/rest/',
                            member_id=bf778c7d11d1fd065521619b61deb0c0,
                            user_id=1,
                            refresh_token=b0a650640061cc84002c4d40000000010000079ab601501b4bcc23261828cd97b2c3cd])).
mock_refresh_response(failure, 401,
                      json([error=invalid_grant,
                            error_description='bad refresh token'])).

:- end_tests(refresh).

:- begin_tests(member_auth,
               [ setup((test_setup,
                        assertz(bitrix24_config:config(app, 'application_id', '1234')),
                        assertz(bitrix24_config:config(app, 'client_secret', '123')),
                        nb_setval(member_refresh_attempts, 0),
                        wrap_predicate(http_client:http_post(Url, Body, Reply, Options), member_refresh,
                                       _Wrapped, mock_member_refresh_http_post(Url, Body, Reply, Options)),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), member_rest,
                                       _WrappedRequest, mock_member_rest_post(Url, Body, Response, Options)))),
                 cleanup((unwrap_predicate(http_client:http_post(Url, Body, Reply, Options), member_refresh),
                          unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), member_rest),
                          test_cleanup)) ]).

test(assert_keys_member_scoped) :-
    bitrix24_auth:assert_keys(portal_a, ['auth[access_token]'='portal-token',
                                         'auth[refresh_token]'='portal-refresh',
                                         'auth[expires]'='4102444800',
                                         'auth[client_endpoint]'='https://portal-a.example/rest/']),
    assertion(bitrix24_config:portal_app_info(portal_a, 'auth[access_token]', 'portal-token')),
    assertion(bitrix24_config:portal_app_info(portal_a, 'auth[client_endpoint]', 'https://portal-a.example/rest/')).

test(member_refresh_updates_only_selected_portal) :-
    bitrix24_auth:assert_keys(portal_a, ['auth[access_token]'='old-portal-token',
                                         'auth[refresh_token]'='portal-refresh',
                                         'auth[expires]'='1',
                                         'auth[client_endpoint]'='https://portal-a.example/rest/']),
    bitrix24_auth:assert_keys(portal_b, ['auth[access_token]'='other-token',
                                         'auth[refresh_token]'='other-refresh',
                                         'auth[expires]'='4102444800',
                                         'auth[client_endpoint]'='https://portal-b.example/rest/']),
    once(bitrix24_auth:refresh_token(portal_a)),
    assertion(bitrix24_config:portal_app_info(portal_a, 'auth[access_token]', member_new_token)),
    assertion(bitrix24_config:portal_app_info(portal_b, 'auth[access_token]', 'other-token')).

test(rest_member_context_uses_portal_token_and_endpoint) :-
    bitrix24_auth:assert_keys(portal_a, ['auth[access_token]'='old-portal-token',
                                         'auth[refresh_token]'='portal-refresh',
                                         'auth[expires]'='1',
                                         'auth[client_endpoint]'='https://portal-a.example/rest/']),
    bitrix24_rest:api_call('app.info', [], Reply, [member(portal_a)]),
    assertion(memberchk('INSTALLED'=true, Reply)).

mock_member_refresh_http_post('https://oauth.bitrix.info/oauth/token/', form(Form), Reply, [status_code(StatusCode)]) :-
    nb_getval(member_refresh_attempts, Attempts0),
    Attempts is Attempts0 + 1,
    nb_setval(member_refresh_attempts, Attempts),
    memberchk(grant_type=refresh_token, Form),
    memberchk(client_id='1234', Form),
    memberchk(client_secret='123', Form),
    memberchk(refresh_token='portal-refresh', Form),
    StatusCode = 200,
    Reply = json([access_token=member_new_token,
                  expires=4102444800,
                  expires_in=3600,
                  scope=app,
                  domain='oauth.bitrix.info',
                  server_endpoint='https://oauth.bitrix.info/rest/',
                  status='L',
                  client_endpoint='https://portal-a.example/rest/',
                  member_id=portal_a,
                  user_id=1,
                  refresh_token=member_refresh_token]).

mock_member_rest_post('https://portal-a.example/rest/app.info', json(Body), Response, _Options) :-
    memberchk(auth=member_new_token, Body),
    Response = ['INSTALLED'=true].

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
                        bitrix24_auth:assert_keys(['auth[access_token]'='old-token',
                                                   'auth[client_endpoint]'='https://portal.example/rest/',
                                                   'auth[expires]'='4102444800',
                                                   'auth[refresh_token]'='refresh-token']),
                        nb_setval(rest_calls, []),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), rest_request,
                                       _WrappedRequest, mock_rest_post(Url, Body, Response, Options)),
                        wrap_predicate(bitrix24_auth:refresh_token, rest_refresh,
                                       _WrappedRefresh, mock_rest_refresh))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), rest_request),
                          unwrap_predicate(bitrix24_auth:refresh_token, rest_refresh),
                          test_cleanup)) ]).

test(expired_token_refreshes_and_retries) :-
    bitrix24_rest:api_call('app.info', [], Reply),
    assertion(memberchk('INSTALLED'=true, Reply)),
    nb_getval(rest_calls, Calls),
    assertion(Calls == ['new-token', 'old-token']).

test(app_info_wrapper) :-
    bitrix24_app:info(Reply),
    assertion(memberchk('INSTALLED'=true, Reply)).

mock_rest_post('https://portal.example/rest/app.info', json(Body), Response, _Options) :-
    memberchk(auth=Token, Body),
    nb_getval(rest_calls, Calls0),
    nb_setval(rest_calls, [Token|Calls0]),
    ( Token == 'old-token'
    -> Response = [error='expired_token', error_description='token expired']
    ;  Response = ['INSTALLED'=true]
    ).

mock_rest_refresh :-
    bitrix24_config:retractall_app_info('auth[access_token]', _),
    bitrix24_config:assert_app_info('auth[access_token]', 'new-token').

:- end_tests(rest).

:- begin_tests(install,
               [ setup((test_setup,
                        assertz(bitrix24_config:config(event, 'ONCRMLEADADD', 'https://handler.example/event?foo=1&bar=2')),
                        assertz(bitrix24_config:config(placement, 'LIST_MENU', ['https://handler.example/widget?title=crm sales', 'Menu & Title'])),
                        nb_setval(install_calls, []),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), install,
                                       _Wrapped, mock_install_post(Url, Body, Response, Options)))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), install),
                          test_cleanup)) ]).

test(install) :-
    bitrix24_regapp:install(
        [ event='ONAPPINSTALL',
          'data[VERSION]'='1',
          'data[ACTIVE]'='Y',
          'data[INSTALLED]'='Y',
          'data[LANGUAGE_ID]'=ru,
          ts='1681295421',
          'auth[access_token]'='install-token',
          'auth[expires]'='4102444800',
          'auth[expires_in]'='3600',
          'auth[scope]'='crm,task,tasks_extended,telephony',
          'auth[domain]'='localhost',
          'auth[server_endpoint]'='https://oauth.bitrix.info/rest/',
          'auth[status]'='L',
          'auth[client_endpoint]'='https://localhost/rest/',
          'auth[member_id]'=bf778c7d11d1fd061521319b61deb0c0,
          'auth[user_id]'='1',
          'auth[refresh_token]'='install-refresh',
          'auth[application_token]'='app-token'
        ]),
    assertion(bitrix24_config:app_info('auth[application_token]', 'app-token')),
    nb_getval(install_calls, Calls),
    assertion(member(call('event.bind', [auth='install-token',event='ONCRMLEADADD',handler='https://handler.example/event?foo=1&bar=2']), Calls)),
    assertion(member(call('placement.bind', [auth='install-token','PLACEMENT'='LIST_MENU','HANDLER'='https://handler.example/widget?title=crm sales','TITLE'='Menu & Title']), Calls)).

mock_install_post(Url, json(Body), Response, _Options) :-
    sub_atom(Url, _, _, 0, Method),
    nb_getval(install_calls, Calls0),
    nb_setval(install_calls, [call(Method, Body)|Calls0]),
    install_response(Method, Response).

install_response('event.get', [[event='ONAPPINSTALL',handler='https://handler.example/install'],
                               [event='ONCRMOLDADD',handler='https://handler.example/old']]).
install_response('event.unbind', [result= @(true)]).
install_response('event.bind', [result= @(true)]).
install_response('placement.get', [[placement='LIST_MENU',handler='https://handler.example/old-widget']]).
install_response('placement.unbind', [result= @(true)]).
install_response('placement.bind', [result= @(true)]).

:- end_tests(install).

:- begin_tests(domain_api,
               [ setup((test_setup,
                        bitrix24_auth:assert_keys(['auth[access_token]'='domain-token',
                                                   'auth[client_endpoint]'='https://portal.example/rest/',
                                                   'auth[expires]'='4102444800',
                                                   'auth[refresh_token]'='refresh-token']),
                        nb_setval(domain_calls, []),
                        wrap_predicate(bitrix24_request:post(Url, Body, Response, Options), domain_api,
                                       _Wrapped, mock_domain_post(Url, Body, Response, Options)))),
                 cleanup((unwrap_predicate(bitrix24_request:post(Url, Body, Response, Options), domain_api),
                          test_cleanup)) ]).

test(user_current_wrapper) :-
    bitrix24_user:current(Reply),
    assertion(memberchk('ID'='1', Reply)),
    nb_getval(domain_calls, Calls),
    assertion(memberchk(call('user.current', [auth='domain-token']), Calls)).

test(user_get_wrapper) :-
    bitrix24_user:get([id=7], Reply),
    assertion(memberchk('ID'='7', Reply)),
    nb_getval(domain_calls, Calls),
    assertion(memberchk(call('user.get', [auth='domain-token',id=7]), Calls)).

test(user_list_wrapper) :-
    bitrix24_user:list([filter=['ACTIVE'=true], select=['ID', 'NAME']], Reply),
    assertion(memberchk('ID'='7', Reply)),
    nb_getval(domain_calls, Calls),
    assertion(memberchk(call('user.get', [auth='domain-token',filter=['ACTIVE'=true],select=['ID', 'NAME']]), Calls)).

test(im_user_get_wrapper) :-
    bitrix24_user:im_user_get(['ID'=7, 'AVATAR_HR'='Y'], Reply),
    assertion(memberchk(avatar_hr='https://portal.example/avatar.png', Reply)),
    nb_getval(domain_calls, Calls),
    assertion(memberchk(call('im.user.get', [auth='domain-token','ID'=7,'AVATAR_HR'='Y']), Calls)).

test(crm_wrappers) :-
    bitrix24_crm:lead_add([fields=['TITLE'='Order #1']], LeadAdd),
    bitrix24_crm:lead_get([id=101], Lead),
    bitrix24_crm:lead_list([select=['ID', 'TITLE']], Leads),
    bitrix24_crm:lead_update(101, ['TITLE'='Updated order'], LeadUpdated),
    bitrix24_crm:activity_add([fields=['SUBJECT'='Message']], ActivityAdd),
    bitrix24_crm:activity_list([filter=['OWNER_ID'=101]], Activities),
    bitrix24_crm:contact_add([fields=['NAME'='Ada']], ContactAdd),
    bitrix24_crm:deal_list([select=['ID', 'TITLE']], Deals),
    bitrix24_crm:contact_list([select=['ID', 'NAME']], Contacts),
    bitrix24_crm:duplicate_find_by_comm([entity_type='CONTACT', type='PHONE', values=['79001234567']], Duplicates),
    bitrix24_crm:lead_contact_add(101, ['CONTACT_ID'=303, 'SORT'=10], LeadContactAdd),
    bitrix24_crm:lead_contact_items_get([id=101], LeadContacts),
    bitrix24_crm:lead_productrows_set(101, [['PRODUCT_ID'=501, 'QUANTITY'=2]], ProductRowsSet),
    bitrix24_crm:product_list([filter=['ACTIVE'='Y']], Products),
    assertion(memberchk(result=101, LeadAdd)),
    assertion(memberchk('ID'='101', Lead)),
    assertion(memberchk('ID'='101', Leads)),
    assertion(memberchk(result= @(true), LeadUpdated)),
    assertion(memberchk(result=401, ActivityAdd)),
    assertion(memberchk('ID'='401', Activities)),
    assertion(memberchk(result=303, ContactAdd)),
    assertion(memberchk('ID'='202', Deals)),
    assertion(memberchk('ID'='303', Contacts)),
    assertion(memberchk('CONTACT'=[303], Duplicates)),
    assertion(memberchk(result= @(true), LeadContactAdd)),
    assertion(memberchk('CONTACT_ID'='303', LeadContacts)),
    assertion(memberchk(result= @(true), ProductRowsSet)),
    assertion(memberchk('ID'='501', Products)).

test(domain_api_error_propagates,
     [throws(error(bitrix24_api_error('crm.deal.list', access_denied, 'access denied', [error='ACCESS_DENIED',error_description='access denied']), _))]) :-
    bitrix24_crm:deal_list([simulate_error=true], _Reply).

test(imconnector_wrappers) :-
    bitrix24_imconnector:list(ListResult),
    bitrix24_imconnector:register(['ID'='aqua_connector'], RegisterResult),
    bitrix24_imconnector:send_messages(['CONNECTOR'='aqua_connector'], SendResult),
    bitrix24_imconnector:connector_data_set(['CONNECTOR'='aqua_connector', 'LINE'=3], DataSetResult),
    bitrix24_imconnector:activate(['CONNECTOR'='aqua_connector', 'LINE'=3, 'ACTIVE'='1'], ActivateResult),
    bitrix24_imconnector:status(['CONNECTOR'='aqua_connector', 'LINE'=3], StatusResult),
    bitrix24_imconnector:send_status_delivery(['CONNECTOR'='aqua_connector'], DeliveryResult),
    assertion(memberchk(aqua_connector=['ID'='aqua_connector'], ListResult)),
    assertion(memberchk(result= @(true), RegisterResult)),
    assertion(memberchk(result= @(true), SendResult)),
    assertion(memberchk(result= @(true), DataSetResult)),
    assertion(memberchk(result= @(true), ActivateResult)),
    assertion(memberchk('CONFIGURED'='Y', StatusResult)),
    assertion(memberchk(result= @(true), DeliveryResult)).

test(imopenlines_wrappers) :-
    bitrix24_imopenlines:config_list_get(['PARAMS'=[select=['ID', 'LINE_NAME']]], Configs),
    bitrix24_imopenlines:crm_chat_get_last_id(['CRM_ENTITY_TYPE'='lead', 'CRM_ENTITY'=101], ChatID),
    bitrix24_imopenlines:bot_session_transfer(['CHAT_ID'=1001, 'USER_ID'=7], TransferResult),
    assertion(memberchk('ID'=3, Configs)),
    assertion(memberchk(result=555, ChatID)),
    assertion(memberchk(result= @(true), TransferResult)).

test(imopenlines_error_propagates,
     [throws(error(bitrix24_api_error('imconnector.status', access_denied, 'connector disabled', [error='ACCESS_DENIED',error_description='connector disabled']), _))]) :-
    bitrix24_imconnector:status(['CONNECTOR'='aqua_connector', simulate_error=true], _Reply).

test(imbot_wrappers) :-
    bitrix24_imbot:bot_register([fields=[code='aqua_bot']], BotRegister),
    bitrix24_imbot:bot_get([code='aqua_bot'], BotGet),
    bitrix24_imbot:chat_add(['TITLE'='Aqua Delivery'], ChatAdd),
    bitrix24_imbot:chat_get(['ENTITY_TYPE'='aqua', 'ENTITY_ID'='main'], ChatGet),
    bitrix24_imbot:chat_user_add(['CHAT_ID'=10, 'USERS'=[7, 8]], ChatUserAdd),
    bitrix24_imbot:chat_message_send([botId=88, dialogId='chat10', fields=[message='Hello']], ChatMessageSend),
    bitrix24_imbot:file_upload([botId=88, dialogId='chat10', fields=[name='report.xls', content='ZmFrZQ==']], FileUpload),
    assertion(memberchk(result=json([bot=json([id=88, code='aqua_bot'])]), BotRegister)),
    assertion(memberchk(result=json([bot=json([id=88, code='aqua_bot'])]), BotGet)),
    assertion(memberchk(result=10, ChatAdd)),
    assertion(memberchk(result=json([id=10, entity_type='aqua', entity_id='main']), ChatGet)),
    assertion(memberchk(result= @(true), ChatUserAdd)),
    assertion(memberchk(result= @(true), ChatMessageSend)),
    assertion(memberchk(result= @(true), FileUpload)).

test(voximplant_wrappers) :-
    bitrix24_voximplant:statistic_get(['FILTER'=['PORTAL_NUMBER'=['79990000000']]], Statistics),
    bitrix24_voximplant:line_get(Lines),
    assertion(memberchk('ID'='901', Statistics)),
    assertion(memberchk('1'='Main line', Lines)).

mock_domain_post(Url, json(Body), Response, _Options) :-
    atom_concat('https://portal.example/rest/', Method, Url),
    nb_getval(domain_calls, Calls0),
    nb_setval(domain_calls, [call(Method, Body)|Calls0]),
    domain_response(Method, Body, Response).

domain_response('user.current', [auth='domain-token'], ['ID'='1', 'NAME'='Test User']).
domain_response('user.get', Body, ['ID'='7', 'NAME'='Ada']) :-
    memberchk(auth='domain-token', Body).
domain_response('im.user.get', Body, [id=7, avatar_hr='https://portal.example/avatar.png']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.add', Body, [result=101]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.get', Body, ['ID'='101', 'TITLE'='Order #1']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.list', Body, ['ID'='101', 'TITLE'='Lead']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.update', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.activity.add', Body, [result=401]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.activity.list', Body, ['ID'='401', 'SUBJECT'='Message']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.contact.add', Body, [result=303]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.deal.list', Body, [error='ACCESS_DENIED', error_description='access denied']) :-
    memberchk(simulate_error=true, Body),
    memberchk(auth='domain-token', Body).
domain_response('crm.deal.list', Body, ['ID'='202', 'TITLE'='Deal']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.contact.list', Body, ['ID'='303', 'NAME'='Contact']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.duplicate.findbycomm', Body, ['CONTACT'=[303]]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.contact.add', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.contact.items.get', Body, ['CONTACT_ID'='303', 'SORT'='10']) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.lead.productrows.set', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('crm.product.list', Body, ['ID'='501', 'NAME'='Water 19L']) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.list', [auth='domain-token'], [aqua_connector=['ID'='aqua_connector']]).
domain_response('imconnector.register', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.send.messages', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.connector.data.set', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.activate', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.status', Body, [error='ACCESS_DENIED', error_description='connector disabled']) :-
    memberchk(simulate_error=true, Body),
    memberchk(auth='domain-token', Body).
domain_response('imconnector.status', Body, ['CONFIGURED'='Y', 'STATUS'='Y']) :-
    memberchk(auth='domain-token', Body).
domain_response('imconnector.send.status.delivery', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imopenlines.config.list.get', Body, ['ID'=3, 'LINE_NAME'='Main line', 'ACTIVE'='Y']) :-
    memberchk(auth='domain-token', Body).
domain_response('imopenlines.crm.chat.getLastId', Body, [result=555]) :-
    memberchk(auth='domain-token', Body).
domain_response('imopenlines.bot.session.transfer', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.v2.Bot.register', Body, [result=json([bot=json([id=88, code='aqua_bot'])])]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.v2.Bot.get', Body, [result=json([bot=json([id=88, code='aqua_bot'])])]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.chat.add', Body, [result=10]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.chat.get', Body, [result=json([id=10, entity_type='aqua', entity_id='main'])]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.chat.user.add', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.v2.Chat.Message.send', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('imbot.v2.File.upload', Body, [result= @(true)]) :-
    memberchk(auth='domain-token', Body).
domain_response('voximplant.statistic.get', Body, ['ID'='901', 'PORTAL_NUMBER'='79990000000']) :-
    memberchk(auth='domain-token', Body).
domain_response('voximplant.line.get', [auth='domain-token'], ['1'='Main line']).

:- end_tests(domain_api).

:- begin_tests(decode, [setup(test_setup), cleanup(test_cleanup)]).

test(decode) :-
    findall(Result,
            bitrix24_utils:decode_response(
                json([result=[json(['ID'='16365','DATE_CREATE'='2018-10-20T05:43:04+03:00']),
                              json(['ID'='16367','DATE_CREATE'='2018-10-20T05:43:35+03:00'])]]),
                Result),
            L),
    assertion(L == [[json(['ID'='16365','DATE_CREATE'='2018-10-20T05:43:04+03:00']),
                     json(['ID'='16367','DATE_CREATE'='2018-10-20T05:43:35+03:00'])]]),
    findall(Remove,
            bitrix24_utils:remove_json([json(['ID'='16365','DATE_CREATE'='2018-10-20T05:43:04+03:00']),
                                        json(['ID'='16367','DATE_CREATE'='2018-10-20T05:43:35+03:00'])],
                                       Remove),
            L2),
    assertion(L2 == [[['ID'='16365','DATE_CREATE'='2018-10-20T05:43:04+03:00'],
                      ['ID'='16367','DATE_CREATE'='2018-10-20T05:43:35+03:00']]]).

:- end_tests(decode).
