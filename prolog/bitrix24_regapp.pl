:- module(bitrix24_regapp, [
              install/1
          ]).

:-use_module(bitrix24_auth).
:-use_module(bitrix24_request).
:-use_module(bitrix24_config).
:-use_module(library(uri)).

install(Data) :-
    bitrix24_auth:assert_keys(Data),
    'event.unbind',
    'event.bind',
    'placement.unbind',
    'placement.bind'.

'event.unbind' :-
    bitrix24_config:app_info('auth[access_token]', AccessToken),
    bitrix24_config:app_info('auth[client_endpoint]', ClientEndpoint),
    format(atom(Url), '~w~w~w', [ClientEndpoint,'event.get?auth=',
                        AccessToken]),
    get(Url, Reply, [status_code(StatusCode)]),
    ( StatusCode == 200
    ->
        forall(member(Xs, Reply), (
               memberchk(event=Event, Xs),
               memberchk(handler=Handler, Xs),
               (Event == 'ONAPPINSTALL'
                -> true
                ;
                    format(atom(UrlU), '~w~w~w~w~w~w~w', [ClientEndpoint,'event.unbind.json?auth=',
                                        AccessToken,'&auth_type=0&event=',Event, '&handler=', Handler]),
                    get(UrlU, ReplyU, []),
                    memberchk(count=1, ReplyU)
               )
           ))
      ;
        debug(log, 'reply ~q : ~q', [Reply, StatusCode])
      ).

'event.bind' :-
    bitrix24_config:app_info('auth[access_token]', AccessToken),
    bitrix24_config:app_info('auth[client_endpoint]', ClientEndpoint),
    forall(config(event, E, H), (
               format(atom(Url), '~w~w~w~w~w~w~w', [ClientEndpoint, 'event.bind.json?auth=', AccessToken, '&event=', E, '&handler=', H]),
               get(Url, Reply, [status_code(StatusCode)]),
               (\+ StatusCode == 200
                ->
                     debug(log, 'reply ~q : ~q', [Reply, StatusCode])
                ;
                    memberchk(result = @(true), Reply)
               )
           )).

'placement.unbind' :-
    bitrix24_config:app_info('auth[access_token]', AccessToken),
    bitrix24_config:app_info('auth[client_endpoint]', ClientEndpoint),
    format(atom(Url), '~w~w~w', [ClientEndpoint, 'placement.get?auth=',
                        AccessToken]),
    get(Url, Reply, [status_code(StatusCode)]),
    ( StatusCode == 200
    ->
        forall(member(Xs, Reply), (
               memberchk(placement=Placement, Xs),
               memberchk(handler=Handler, Xs),
               format(atom(UrlU), '~wplacement.unbind/?auth=~w&auth_type=0&placement=~w&handler=~w', [ClientEndpoint, AccessToken, Placement, Handler]),
               get(UrlU, ReplyU, []),
               memberchk(count=_C, ReplyU)
               )
              )
      ;
        debug(log, 'reply ~q : ~q', [Reply, StatusCode])
      ).

'placement.bind' :-
    bitrix24_config:app_info('auth[access_token]', AccessToken),
    bitrix24_config:app_info('auth[client_endpoint]', ClientEndpoint),
    forall(config(placement, E, [H, T]), (
               format(atom(Url), '~wplacement.bind/?auth=~w&placement=~w&handler=~w&title=~w', [ClientEndpoint, AccessToken, E, H, T]),
               uri:urinormalized(Url, UrlNormalized),
               get(UrlNormalized, Reply, [status_code(StatusCode)]),
               (\+ StatusCode == 200
                ->
                     debug(log, 'reply ~q : ~q', [Reply, StatusCode])
                ;
                    memberchk(result = @(true), Reply)
               )
           )).
