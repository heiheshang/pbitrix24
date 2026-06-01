:- module(bitrix24_install, [
          payload_context/3,
          payload_contexts/2,
          save_payload_contexts/3
      ]).

:- use_module(library(json)).

payload_context(Payload, ContextRef, Context) :-
    normalize_install_payload(Payload, Normalized),
    extract_context_ref(Normalized, ContextRef),
    build_context(Normalized, Context).

payload_contexts(Payload, ContextPairs) :-
    payload_context(Payload, ContextRef, Context),
    ContextPairs = [ContextRef-Context].

save_payload_contexts(Provider, Payload, ContextPairs) :-
    payload_contexts(Payload, ContextPairs),
    forall(member(ContextRef-Context, ContextPairs),
           call(Provider:save_context(ContextRef, Context))).

normalize_install_payload(Payload, Payload) :-
    is_dict(Payload),
    !.
normalize_install_payload(form(Pairs), Dict) :-
    !,
    pairs_to_dict(Pairs, Dict).
normalize_install_payload(Pairs, Dict) :-
    is_list(Pairs),
    pairs_to_dict(Pairs, Dict).

pairs_to_dict(Pairs, Dict) :-
    findall(Key-Value,
            ( member(Pair, Pairs),
              pair_key_value(Pair, Key, Value)
            ),
            DictPairs),
    dict_pairs(Dict, install, DictPairs).

pair_key_value(Key=Value, Key, Value).
pair_key_value(Key-Value, Key, Value).

extract_context_ref(Payload, member(MemberID)) :-
    payload_value(Payload, 'auth[member_id]', MemberID0),
    normalize_member_id(MemberID0, MemberID),
    !.
extract_context_ref(_Payload, global).

build_context(Payload, Context) :-
    derive_expires_at(Payload, ExpiresAt),
    payload_optional_value(Payload, 'auth[member_id]', MemberID),
    payload_value(Payload, 'auth[access_token]', AccessToken),
    payload_value(Payload, 'auth[refresh_token]', RefreshToken),
    payload_value(Payload, 'auth[client_endpoint]', ClientEndpoint),
    payload_optional_value(Payload, 'auth[server_endpoint]', ServerEndpoint),
    payload_optional_value(Payload, 'auth[application_token]', ApplicationToken),
    payload_optional_value(Payload, 'auth[user_id]', UserID),
    payload_optional_value(Payload, 'auth[domain]', Domain),
    Context = _{
        member_id: MemberID,
        access_token: AccessToken,
        refresh_token: RefreshToken,
        expires_at: ExpiresAt,
        client_endpoint: ClientEndpoint,
        server_endpoint: ServerEndpoint,
        application_token: ApplicationToken,
        user_id: UserID,
        domain: Domain
    }.

derive_expires_at(Payload, ExpiresAt) :-
    payload_optional_value(Payload, 'auth[expires]', ExpiresValue),
    normalize_optional_integer(ExpiresValue, ExpiresAt),
    ExpiresAt > 0,
    !.
derive_expires_at(Payload, ExpiresAt) :-
    payload_value(Payload, 'auth[expires_in]', ExpiresInValue),
    normalize_integer(ExpiresInValue, ExpiresIn),
    get_time(Now),
    ExpiresAt is floor(Now) + ExpiresIn.

payload_value(Payload, Key, Value) :-
    atom(Key),
    get_dict(Key, Payload, Value),
    !.
payload_value(Payload, Key, Value) :-
    string(Key),
    atom_string(KeyAtom, Key),
    get_dict(KeyAtom, Payload, Value).

payload_optional_value(Payload, Key, Value) :-
    payload_value(Payload, Key, Value),
    !.
payload_optional_value(_Payload, _Key, '').

normalize_member_id(Value, MemberID) :-
    string(Value),
    !,
    atom_string(MemberID, Value).
normalize_member_id(Value, MemberID) :-
    atom(Value),
    !,
    MemberID = Value.
normalize_member_id(Value, MemberID) :-
    term_string(Value, MemberString),
    atom_string(MemberID, MemberString).

normalize_optional_integer('', 0) :-
    !.
normalize_optional_integer(Value, Integer) :-
    normalize_integer(Value, Integer).

normalize_integer(Value, Integer) :-
    integer(Value),
    !,
    Integer = Value.
normalize_integer(Value, Integer) :-
    number(Value),
    !,
    Integer is floor(Value).
normalize_integer(Value, Integer) :-
    string(Value),
    !,
    number_string(Integer, Value).
normalize_integer(Value, Integer) :-
    atom(Value),
    atom_number(Value, Integer).
