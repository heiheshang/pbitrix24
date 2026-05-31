:- module(bitrix24_imopenlines, [
             config_list_get/2,
             crm_chat_get_last_id/2,
             bot_session_transfer/2
         ]).

:- use_module(bitrix24_rest).

config_list_get(Params, Result) :-
    bitrix24_rest:api_call('imopenlines.config.list.get', Params, Result).

crm_chat_get_last_id(Params, Result) :-
    bitrix24_rest:api_call('imopenlines.crm.chat.getLastId', Params, Result).

bot_session_transfer(Params, Result) :-
    bitrix24_rest:api_call('imopenlines.bot.session.transfer', Params, Result).
