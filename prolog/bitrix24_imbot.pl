:- module(bitrix24_imbot, [
             bot_register/3,
             bot_register/4,
             bot_get/3,
             bot_get/4,
             chat_add/3,
             chat_add/4,
             chat_get/3,
             chat_get/4,
             chat_user_add/3,
             chat_user_add/4,
             chat_message_send/3,
             chat_message_send/4,
             file_upload/3,
             file_upload/4
         ]).

:- use_module(bitrix24_rest).

bot_register(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.v2.Bot.register', Params, Result).

bot_register(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.v2.Bot.register', Params, Result).

bot_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.v2.Bot.get', Params, Result).

bot_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.v2.Bot.get', Params, Result).

chat_add(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.chat.add', Params, Result).

chat_add(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.chat.add', Params, Result).

chat_get(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.chat.get', Params, Result).

chat_get(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.chat.get', Params, Result).

chat_user_add(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.chat.user.add', Params, Result).

chat_user_add(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.chat.user.add', Params, Result).

chat_message_send(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.v2.Chat.Message.send', Params, Result).

chat_message_send(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.v2.Chat.Message.send', Params, Result).

file_upload(Provider, Params, Result) :-
    bitrix24_rest:api_call(Provider, 'imbot.v2.File.upload', Params, Result).

file_upload(Provider, ContextRef, Params, Result) :-
    bitrix24_rest:api_call(Provider, ContextRef, 'imbot.v2.File.upload', Params, Result).
