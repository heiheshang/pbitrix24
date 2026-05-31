:- module(bitrix24_imconnector, [
             list/1,
             register/2,
             send_messages/2,
             connector_data_set/2,
             activate/2,
             status/2,
             send_status_delivery/2
         ]).

:- use_module(bitrix24_rest).

list(Result) :-
    bitrix24_rest:api_call('imconnector.list', [], Result).

register(Params, Result) :-
    bitrix24_rest:api_call('imconnector.register', Params, Result).

send_messages(Params, Result) :-
    bitrix24_rest:api_call('imconnector.send.messages', Params, Result).

connector_data_set(Params, Result) :-
    bitrix24_rest:api_call('imconnector.connector.data.set', Params, Result).

activate(Params, Result) :-
    bitrix24_rest:api_call('imconnector.activate', Params, Result).

status(Params, Result) :-
    bitrix24_rest:api_call('imconnector.status', Params, Result).

send_status_delivery(Params, Result) :-
    bitrix24_rest:api_call('imconnector.send.status.delivery', Params, Result).
