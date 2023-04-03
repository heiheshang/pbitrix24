:- module(test_bitrix24_auth, [
              test_bitrix24_auth/0
          ]).

:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(plunit)).
:- use_module(bitrix24_auth).

:- set_prolog_flag(plunit_output, always).

test_bitrix24_auth :-
    run_tests([
               assert_keys,
               refresh
              ]).

:- begin_tests(assert_keys, [setup((absolute_file_name(.,Path),
                                    bitrix24_auth:open_db(Path))),
                             cleanup(retractall(bitrix24_auth:config(_,_,_)))]).

test(assert_keys) :-
       absolute_file_name(.,Path),
       bitrix24_auth:open_db(Path),
       Data = ['data[VERSION]'='1','data[ACTIVE]'='Y','data[INSTALLED]'='Y','data[LANGUAGE_ID]'=ru,ts='1677593645','auth[access_token]'='3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6','auth[expires]'='1677597245','auth[expires_in]'='3600','auth[scope]'=crm,'auth[domain]'='test.bitrix24.ru','auth[server_endpoint]'='https://oauth.bitrix.info/rest/','auth[status]'='L','auth[client_endpoint]'='https://test.bitrix24.ru/rest/','auth[member_id]'=bf778c7d11d1fd067221619b61deb0c0,'auth[user_id]'='1','auth[refresh_token]'='2d9925640061aca0002c4d4000000001000007a98d6991b6116d0be1c4982a6a535a38','auth[application_token]'='470fca5b93a683685254564bf32eafbcaf'],
       bitrix24_auth:assert_keys(Data),
       bitrix24_auth:app_info('auth[access_token]', '3d1afe630061aca0002c4d4034500001000007aea83200862f920335a82df6035c81f6').

:- end_tests(assert_keys).

:- begin_tests(refresh, [setup((absolute_file_name(.,Path),
                                bitrix24_auth:open_db(Path),
                                assert(bitrix24_auth:config(app, 'application_id', '1234')),
                                assert(bitrix24_auth:config(app, 'client_secret', '123')),
                                wrap_predicate(http_client:http_get(Url, Reply, Options), refresh,
                                             _Wrapped, mock_http_get(Url, Reply, Options)))),
                         cleanup((retractall(bitrix24_auth:config(_,_,_)),
                                 unwrap_predicate(http_client:http_get(Url, Reply, Options), refresh)))]).

test(refresh) :-
    Data = [access_token='69b4fe630061aca0002c4d4000003681000007fd7ac2b201ddab0a6243287a5c561a50',expires=1677636713,expires_in=3600,scope=app,domain='oauth.bitrix.info',server_endpoint='https://oauth.bitrix.info/rest/',status='L',client_endpoint='https://test.bitrix24.ru/rest/',member_id=bf778c7d11d1fd065547919b61deb0c0,user_id=1,refresh_token='593326640061aca0002c4d40048600015670007e15f25b78deb2ce06bd959e16225c15d'],
    bitrix24_auth:assert_keys(Data),
    bitrix24_auth:assert_app_info('auth[access_token]', '69b4fe630061aca0002c4d4000003681000007fd7ac2b201ddab0a6243287a5c561a50'),
    bitrix24_auth:refresh_token,
    bitrix24_auth:app_info('auth[access_token]', c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0).

http_client:mock_http_get(_URL, Reply, [status_code(200)]) :- !,
    Reply = json([access_token=c02729640061cc84002c4d40000000010000073597cfbedf6b41e578215f8b6d7c9ff0,expires=1680418752,expires_in=3600,scope=app,domain='oauth.bitrix.info',server_endpoint='https://oauth.bitrix.info/rest/',status='L',client_endpoint='https://test.bitrix24.ru/rest/',member_id=bf778c7d11d1fd065521619b61deb0c0,user_id=1,refresh_token=b0a650640061cc84002c4d40000000010000079ab601501b4bcc23261828cd97b2c3cd]).

:- end_tests(refresh).
