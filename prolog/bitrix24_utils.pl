:-module(bitrix24_utils, [
             decode_response/2,
             remove_json/2
         ]).

decode_response(json(Reply), Result) :-
    memberchk(result = json(Result), Reply),!.
decode_response(json(Reply), Result) :-
    memberchk(result = Result, Reply),
    is_list(Result),!.
decode_response(json(Reply), Reply).

remove_json([], []).
remove_json([json(Term)|T], [Term|NewT]) :-
    remove_json(T, NewT).
remove_json([H|T], [H|NewT]) :-
    H \= json(_),
    remove_json(T, NewT).
