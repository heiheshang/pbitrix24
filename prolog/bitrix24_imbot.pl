:- module(bitrix24_imbot, [
             bot_register/2,
             bot_get/2,
             chat_add/2,
             chat_get/2,
             chat_user_add/2,
             chat_message_send/2,
             file_upload/2
         ]).

:- use_module(bitrix24_rest).

bot_register(Params, Result) :-
    bitrix24_rest:api_call('imbot.v2.Bot.register', Params, Result).

bot_get(Params, Result) :-
    bitrix24_rest:api_call('imbot.v2.Bot.get', Params, Result).

chat_add(Params, Result) :-
    bitrix24_rest:api_call('imbot.chat.add', Params, Result).

chat_get(Params, Result) :-
    bitrix24_rest:api_call('imbot.chat.get', Params, Result).

chat_user_add(Params, Result) :-
    bitrix24_rest:api_call('imbot.chat.user.add', Params, Result).

chat_message_send(Params, Result) :-
    bitrix24_rest:api_call('imbot.v2.Chat.Message.send', Params, Result).

file_upload(Params, Result) :-
    bitrix24_rest:api_call('imbot.v2.File.upload', Params, Result).
