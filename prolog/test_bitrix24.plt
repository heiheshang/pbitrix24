:- module(test_bitrix24, [
              test_bitrix24/0
          ]).

:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(plunit)).
:- use_module(bitrix24_auth).
:- use_module(bitrix24_request).
:- use_module(bitrix24_regapp).
:- use_module(bitrix24_utils).
:- use_module(bitrix24_config).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(debug)).

:- debug(log).
:- set_prolog_flag(plunit_output, always).

test_bitrix24 :-
    run_tests([
               assert_keys,
               refresh,
               post,
               install
              ]).

:- begin_tests(assert_keys, [setup((absolute_file_name(.,Path),
                                    bitrix24_config:open_db(Path))),
                             cleanup(retractall(bitrix24_config:config(_,_,_)))]).

test(assert_keys) :-
       absolute_file_name(.,Path),
       bitrix24_config:open_db(Path),
       Data = ['data[VERSION]'='1','data[ACTIVE]'='Y','data[INSTALLED]'='Y','data[LANGUAGE_ID]'=ru,ts='1677593645','auth[access_token]'='3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6','auth[expires]'='1677597245','auth[expires_in]'='3600','auth[scope]'=crm,'auth[domain]'='test.bitrix24.ru','auth[server_endpoint]'='https://oauth.bitrix.info/rest/','auth[status]'='L','auth[client_endpoint]'='https://test.bitrix24.ru/rest/','auth[member_id]'=bf778c7d11d1fd067221619b61deb0c0,'auth[user_id]'='1','auth[refresh_token]'='2d9925640061aca0002c4d4000000001000007a98d6991b6116d0be1c4982a6a535a38','auth[application_token]'='470fca5b93a683685254564bf32eafbcaf'],
       bitrix24_auth:assert_keys(Data),
       bitrix24_config:app_info('auth[access_token]', '3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6').

:- end_tests(assert_keys).

:- begin_tests(refresh, [setup((absolute_file_name(.,Path),
                                bitrix24_config:open_db(Path),
                                assert(bitrix24_config:config(app, 'application_id', '1234')),
                                assert(bitrix24_config:config(app, 'client_secret', '123')),
                                wrap_predicate(http_client:http_get(Url, Reply, Options), refresh,
                                             _Wrapped, mock_http_get(Url, Reply, Options)))),
                         cleanup((retractall(bitrix24_config:config(_,_,_)),
                                 unwrap_predicate(http_client:http_get(Url, Reply, Options), refresh)))]).

test(refresh) :-
    Data = [access_token='69b4fe630061aca0002c4d4000003681000007fd7ac2b201ddab0a6243287a5c561a50',expires=1677636713,expires_in=3600,scope=app,domain='oauth.bitrix.info',server_endpoint='https://oauth.bitrix.info/rest/',status='L',client_endpoint='https://test.bitrix24.ru/rest/',member_id=bf778c7d11d1fd065547919b61deb0c0,user_id=1,refresh_token='593326640061aca0002c4d40048600015670007e15f25b78deb2ce06bd959e16225c15d'],
    bitrix24_auth:assert_keys(Data),
    bitrix24_config:assert_app_info('auth[access_token]', '69b4fe630061aca0002c4d4000003681000007fd7ac2b201ddab0a6243287a5c561a50'),
    bitrix24_auth:refresh_token,
    bitrix24_config:app_info('auth[access_token]', c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0).

http_client:mock_http_get(_URL, Reply, [status_code(200)]) :- !,
    Reply = json([access_token=c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0,expires=1680418752,expires_in=3600,
                  scope=app,domain='oauth.bitrix.info',server_endpoint='https://oauth.bitrix.info/rest/',status='L',
                  client_endpoint='http://localhost/rest/',member_id=bf778c7d11d1fd065521619b61deb0c0,user_id=1,
                  refresh_token=b0a650640061cc84002c4d40000000010000079ab601501b4bcc23261828cd97b2c3cd]).

:- end_tests(refresh).

:- begin_tests(post).

test(post) :-
    request('/post',json(json([id = 1])), Reply),
    memberchk('ID'='998145', Reply).

request(Path, Body, Reply) :-
    setup_call_cleanup(
     http_server(http_dispatch, [port(localhost:Port)]),
     ( format(atom(Url), 'http://localhost:~w~w', [Port, Path]),
        post(Url, Body, [], Reply)
     ),
     http_stop_server(Port, [])).

:- http_handler(root(post), post, [methods([post])]).

post(Request) :-
    http_read_json_dict(Request, _Dict),
    Reply = json([result =
                  json(['ID'='998145', 'NAME'= @(null),'SOURCE_DESCRIPTION'='Звонок поступил на номер: МТС',
                        'STATUS_ID'='NEW','STATUS_DESCRIPTION'= @(null),'POST'= @(null),'COMMENTS'= @(null),
                        'CURRENCY_ID'='RUB','OPPORTUNITY'='170.00','IS_MANUAL_OPPORTUNITY'='N','HAS_PHONE'='Y',
                        'HAS_EMAIL'='N','HAS_IMOL'='N','ASSIGNED_BY_ID'='25','CREATED_BY_ID'='25','MODIFY_BY_ID'='1',
                        'DATE_CREATE'='2023-02-24T12:54:14+03:00','DATE_MODIFY'='2023-02-24T12:54:15+03:00',
                        'DATE_CLOSED'='','STATUS_SEMANTIC_ID'='P','OPENED'='Y','ORIGINATOR_ID'= @(null),'ORIGIN_ID'= @(null),
                        'MOVED_BY_ID'='25','MOVED_TIME'='2023-02-24T12:54:14+03:00','ADDRESS'= @(null),'ADDRESS_2'= @(null),
                        'ADDRESS_CITY'= @(null),'ADDRESS_POSTAL_CODE'= @(null),'ADDRESS_REGION'= @(null),'ADDRESS_PROVINCE'= @(null),
                        'ADDRESS_COUNTRY'= @(null),'ADDRESS_COUNTRY_CODE'= @(null),'ADDRESS_LOC_ADDR_ID'= @(null),'UTM_SOURCE'= @(null),
                        'UTM_MEDIUM'= @(null),'UTM_CAMPAIGN'= @(null),'UTM_CONTENT'= @(null),'UTM_TERM'= @(null),
                        'LAST_ACTIVITY_BY'='25','LAST_ACTIVITY_TIME'='2023-02-24T12:54:13+03:00','UF_CRM_1540389281507'='',
                        'UF_CRM_1551340601'='','UF_CRM_PRODUCT'='','UF_CRM_FORMNAME'='','UF_CRM_COOKIES'='','UF_CRM_DELIVERY'='',
                        'UF_CRM_DELIVERYPRICE'='','UF_CRM_DELIVERYADDRE'='','UF_CRM_CHECKBOX'='','UF_CRM_PAYMENTSYSTEM'='','UF_CRM_COMMENTS'='',
                        'UF_CRM_TEXTAREA'='','UF_CRM_TRANID'='','UF_CRM_PAYMENTID'='','UF_CRM_COMPANY_TITLE'='','UF_CRM_INN'='','UF_CRM_KPP'='',
                        'UF_CRM_BIK'='','UF_CRM_BANKNAME'='','UF_CRM_BANKACCOUNT'='','UF_CRM_CORACCOUNT'='','UF_CRM_ORDERID'='','UF_CRM_SUBTOTAL'='',
                        'UF_CRM_INSTAGRAM_WZ'='','UF_CRM_VK_WZ'='','UF_CRM_AVITO_WZ'='','UF_CRM_TELEGRAMUSERNAME_WZ'='','UF_CRM_TELEGRAMID_WZ'='',
                        'PHONE'=[json(['ID'='1042433','VALUE_TYPE'='WORK','VALUE'='9143926007','TYPE_ID'='PHONE'])]]),
                  time=json([start=1677233302.798346,finish=1677233302.892825,duration=0.09447884559631348,processing=0.03690004348754883,
                            date_start='2023-02-24T13:08:22+03:00',date_finish='2023-02-24T13:08:22+03:00',operating_reset_at=1677233902,operating=0])]),
    reply_json(Reply).

:- end_tests(post).

:- begin_tests(install, [setup((
                               wrap_predicate(http_client:http_get(Url, Reply, Options), install,
                                             _Wrapped, mock_http_get_install(Url, Reply, Options)),
                                   assert(bitrix24_config:config(event, onCrmAdd, 'http://localhost/rest')))),
                        cleanup((
                                 unwrap_predicate(http_client:http_get(Url, Reply, Options), install),
                                 retract(bitrix24_config:config(event, _, _))
                                 ))]).



test(install) :-
    bitrix24_regapp:install(
                     [event='ONAPPINSTALL','data[VERSION]'='1','data[ACTIVE]'='Y','data[INSTALLED]'='Y','data[LANGUAGE_ID]'=ru,
                      ts='1681295421','auth[access_token]'='4d9636640061cc84002c4d40000000010000078db61a43f077dd35398b6201f7795438',
                      'auth[expires]'='1681299021','auth[expires_in]'='3600','auth[scope]'='crm,task,tasks_extended,telephony',
                      'auth[domain]'='localhost','auth[server_endpoint]'='https://oauth.bitrix.info/rest/','auth[status]'='L',
                      'auth[client_endpoint]'='https://localhost/rest/','auth[member_id]'=bf778c7d11d1fd061521319b61deb0c0,
                      'auth[user_id]'='1','auth[refresh_token]'='3d155e640061cc84002c4d4000000001000007c025cd8cbffb66a5881d6117b91fff3a',
                      'auth[application_token]'='2b47d67465bff3392140fca1b0317ec9']),
    bitrix24_config:app_info('auth[refresh_token]', RefreshToken),
    bitrix24_config:app_info('auth[application_token]', AppToken),
    assertion(RefreshToken == '3d155e640061cc84002c4d4000000001000007c025cd8cbffb66a5881d6117b91fff3a'),
    assertion(AppToken == '2b47d67465bff3392140fca1b0317ec9').

http_client:mock_http_get_install(_Url, Reply, [status_code(200)]) :- !,
    Reply = json([result = @(true)]).

:- end_tests(install).

html_content(Type, Content, Needle) :-
    http_parse_header_value(content_type, Type, media(text/html, _Attributes)),
    contains_codes(Needle, Content).

contains_codes(Needle, Haystack) :-
    to_string(Needle, NeedleS),
    to_string(Haystack, HaystackS),
    sub_string(HaystackS, _, _, _, NeedleS),
    !.

to_string(S, S) :-
    string(S),
    !.
to_string(A, S) :-
    atom(A),
    !,
    atom_string(A, S).
to_string(Codes, S) :-
    string_codes(S, Codes).
