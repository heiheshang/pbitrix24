:-module(bitrix24_utils, [
             decode_response/2
         ]).

decode_response(json(Reply), Result) :-
    memberchk(result = json(Result), Reply),!.
decode_response(json(Reply), Result) :-
    memberchk(result = Result, Reply),
    is_list(Result),!.
decode_response(json(Reply), Reply).
