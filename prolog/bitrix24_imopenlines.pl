:- module(bitrix24_imopenlines, [
             config_list_get/3,
             config_list_get/4,
             crm_chat_get_last_id/3,
             crm_chat_get_last_id/4,
             bot_session_transfer/3,
             bot_session_transfer/4
         ]).

:- use_module(bitrix24_rest).

config_list_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imopenlines.config.list.get', Params, Result).

config_list_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imopenlines.config.list.get', Params, Result).

crm_chat_get_last_id(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imopenlines.crm.chat.getLastId', Params, Result).

crm_chat_get_last_id(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imopenlines.crm.chat.getLastId', Params, Result).

bot_session_transfer(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imopenlines.bot.session.transfer', Params, Result).

bot_session_transfer(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imopenlines.bot.session.transfer', Params, Result).
