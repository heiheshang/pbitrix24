:- module(bitrix24_regapp, [
              install/1
          ]).

:-use_module(bitrix24_auth).
:-use_module(bitrix24_request).

install(Data) :-
    assert_keys(Data),
    'event.unbind',
    'event.bind'.

'event.unbind' :-
    bitrix24_auth:app_info('auth[access_token]', AccessToken),
    bitrix24_auth:app_info('auth[client_endpoint]', ClientEndpoint),
    format(atom(Url), '~w~w~w', [ClientEndpoint,'event.get?auth=',
                        AccessToken]),
    catch(
        get(Url, Reply, [status_code(StatusCode)]),
        E,
        throw(error(http_open_error(E),_))),
    ( StatusCode == 200
    ->
        forall(member(json(Xs), Reply), (
               memberchk(event=Event, Xs),
               memberchk(handler=Handler, Xs),
               (Event == 'ONAPPINSTALL'
                -> true
                ;
                    format(atom(UrlU), '~w~w~w~w~w~w~w', [ClientEndpoint,'event.unbind.json?auth=',
                                        AccessToken,'&auth_type=0&event=',Event, '&handler=', Handler]),
                    get(UrlU, ReplyU, []),
                    memberchk(result = @(true), ReplyU)
               )
           ))
      ;
        debug(log, 'reply ~q : ~q', [Reply, StatusCode])
      ).

'event.bind' :-
    bitrix24_auth:app_info('auth[access_token]', AccessToken),
    bitrix24_auth:app_info('auth[client_endpoint]', ClientEndpoint),
    forall(config(event, E, H), (
               %%atomic_list_concat([ClientEndpoint, 'event.bind.json?auth=', AccessToken, '&event=', E, '&handler=', H], URL),
               format(atom(Url), '~w~w~w~w~w~w~w', [ClientEndpoint, 'event.bind.json?auth=', AccessToken, '&event=', E, '&handler=', H]),
               catch(
                   get(Url, Reply, [status_code(StatusCode)]),
                   E,
                   throw(error(http_open_error(E),_))),
               (\+ StatusCode == 200
                ->
                     debug(log, 'reply ~q : ~q', [Reply, StatusCode])
                ;
                    memberchk(result = @(true), Reply)
               )
           )).
