:- module(bitrix24_config, [
              load_config/1,
              config/3,
              app_info/2,
              portal_app_info/3,
              assert_portal_app_info/3,
              retractall_portal_app_info/3,
              open_db/1,
              catalog_app/1
          ]).

:- use_module(library(persistency)).

 :- dynamic config/3.

:- persistent
  app_info(key:atom, value:any),
  portal_app_info(member_id:atom, key:atom, value:any).

:- setting(database, callable, 'app_info.db',
           "Данные приложения полученные при установке").

load_config(Dir) :-
    atom_concat(Dir, '{*.yaml}', TemplateFiles),
    expand_file_name(TemplateFiles, Files),
    ( length(Files, Len), Len \== 0
    ->
      load_yaml_config(Files)
      ;  throw(error(config,
                    context(Dir),
                    'Отсутствуют конфигурационные файлы'))
    ).

load_yaml_config(Fs) :-
    forall(member(F, Fs),
           (
               yaml_read(F, Yaml),
               dict_pairs(Yaml, _Tag, Xs),
               save_config(Xs)
           )).

save_config([]).
save_config([Tag-Yaml|Xs]) :-
    dict_pairs(Yaml, _, Ys),
    forall(member(Key - Value, Ys),
           (normalize_config_value(Value, AValue),
           retractall(config(Tag, Key, _)),
           assertz(config(Tag, Key, AValue))
          )
        ),
    save_config(Xs).

normalize_config_value(Value, AtomValue) :-
    string(Value),
    !,
    string_to_atom(Value, AtomValue).
normalize_config_value([], []) :-
    !.
normalize_config_value([X|Xs], [Y|Ys]) :-
    !,
    normalize_config_value(X, Y),
    normalize_config_value(Xs, Ys).
normalize_config_value(Value, Value).

catalog_app(Dir) :-
    exists_directory(Dir).

catalog_app(Dir) :-
    make_directory(Dir).

open_db(_) :-
    db_attached(_),
    !.
open_db(Dir) :-
    setting(database, Spec),
    atom_concat(Dir, Spec, PathFile),
    db_attach(PathFile, [sync(close)]), !.
