:- module(bitrix24_imconnector, [
             list/2,
             list/3,
             register/3,
             register/4,
             send_messages/3,
             send_messages/4,
             connector_data_set/3,
             connector_data_set/4,
             activate/3,
             activate/4,
             status/3,
             status/4,
             send_status_delivery/3,
             send_status_delivery/4
         ]).

:- use_module(bitrix24_rest).

list(Provider, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.list', [], Result).

list(Provider, ContextRef, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.list', [], Result).

register(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.register', Params, Result).

register(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.register', Params, Result).

send_messages(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.send.messages', Params, Result).

send_messages(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.send.messages', Params, Result).

connector_data_set(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.connector.data.set', Params, Result).

connector_data_set(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.connector.data.set', Params, Result).

activate(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.activate', Params, Result).

activate(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.activate', Params, Result).

status(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.status', Params, Result).

status(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.status', Params, Result).

send_status_delivery(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imconnector.send.status.delivery', Params, Result).

send_status_delivery(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imconnector.send.status.delivery', Params, Result).
