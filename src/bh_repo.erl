%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 10:58 PM
%%%-------------------------------------------------------------------
-module(bh_repo).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-author("simon").

%% API
-export([]).

%% callbacks
-callback table_config() -> map().
-callback get(Repo :: tuple(), Key :: atom()) -> any().
-callback get(Repo :: tuple(), Key :: atom(), Default :: any()) -> any().


%% api
-export([
  init/1
  , data_init/0
  , drop_index/1
  , create_index/1
  , recreate_index/1

  , create_pk/3
  , create/3
  , read/2
  , read_index/2
  , read_index/3
  , save/1
  , save/2
  , save/3
  , update/3
  , update_create/3
  , update_pk/3
  , update_create_pk/3

  , get_all/1
  , all_pks/1

  , query/2
  , query/3
  , qh/2
  , e/1


  , bindings_2_filter_cond/2

  %%%%
  , fetch_index/2
  , fetch/2
  , fetch_model/2
  , fetch_index_model/2

  , fetch_by/3
  , fetch_by_index/3

  %%%
  , dump_all_to_file/1
  , dump_all_to_file_safe/1
  , dump_to_file/2
  , load_from_file/2
  , load_all_from_file/1

  %%
  , drop_table/1

  %%
  , transfer_history_log/2
  , transfer_history_log/3
  , delete/1
  , delete/2


]).

-compile(export_all).
%%-------------------------------------------------------------------
%% utils,


%% db(mnesia) interface
save(Repo) when is_tuple(Repo) ->
  save(Repo, [dirty]),
  lager:debug("Write ~p to mnesia successful!", [Repo]),
  ok.

save(M, Model) when is_atom(M), is_map(Model) ->
  Repo = pg_repos:from_model(M, Model),
  save(Repo);
save(Repo, [sync]) when is_tuple(Repo) ->
  F = fun
        () ->
          ok = mnesia:write(Repo),
          ok
      end,
  {atomic, ok} = mnesia:sync_transaction(F),
  ok = mnesia:sync_log(),
%%  lager:debug("Write ~p to mnesia successful!", [Repo]),
  ok;
save(Repo, [dirty]) when is_tuple(Repo) ->
  ok = mnesia:dirty_write(Repo),
%%  lager:debug("Write ~p to mnesia successful!", [Repo]),
  ok.


save(M, Model, Options) when is_atom(M), is_map(Model), is_list(Options) ->
  Repo = pg_repos:from_model(M, Model),
  save(Repo, Options).

%%-------------------------------------------------------------------
delete(Repo) when is_tuple(Repo) ->
  ok = mnesia:dirty_delete_object(Repo),
  ok.

delete(M, PK) when is_atom(M) ->
  TableName = pg_repos:table_name(M),
  ok = mnesia:dirty_delete(TableName, PK),
  ok.


%%-------------------------------------------------------------------
is_pk_value_exist(M, PkValue) when is_atom(M) ->
  Repo = read(M, PkValue),
  [] =/= Repo.

is_index_value_exist(M, IndexName, IndexValue) when is_atom(M), is_atom(IndexName) ->
  Repo = read_index(M, IndexName, IndexValue),
  [] =/= Repo.

next_pk_sequence_id(M) when is_atom(M) ->
%%  IsSequence = maps:get(pk_is_sequence, apply(M, table_config, []), true),
  IsSequence = maps:get(pk_is_sequence, M: table_config(), true),
  get_next_id(M, IsSequence).

get_next_id(_, false) ->
  [];
get_next_id(M, true) ->
  %% get next id from table pk=id
  IdList = all_pks(M),
  NextID = pg_repos:next_id(IdList),
  NextID.

all_pks(M) ->
  TableName = pg_repos:table_name(M),
  PKList = mnesia:dirty_all_keys(TableName),
  PKList.

table_config(Key, M) when is_atom(Key), is_atom(M) ->
%%  Config = apply(M, table_config, []),
  Config = M: table_config(),
  maps:get(Key, Config, undefined).

table_config_pk(M) when is_atom(M) ->
  table_config(pk_key_name, M).
%%-------------------------------------------------------------------
clean_up_record_list(M, List) when is_atom(M), is_list(List) ->
  RecFields = pg_repos:fields(M),
  AtomFields = [payment_method, status],
  TSFields = [update_ts, last_login_ts, last_update_ts],

  F = fun(Field, Acc) ->
%%    lager:debug("Field = ~p,Acc = ~p", [Field, Acc]),
    FieldNew1 = kv_clean_record_only(Field, RecFields),
%%    lager:debug("FieldNew1 = ~p", [FieldNew1]),
    FieldNew2 = kv_clean_convert_atom(FieldNew1, AtomFields),
%%    lager:debug("FieldNew2 = ~p", [FieldNew2]),
    FieldNew3 = kv_clean_convert_ts(FieldNew2, TSFields),
%%    lager:debug("FieldNew3 = ~p", [FieldNew3]),
    case FieldNew3 of
      [] ->
        Acc;
      FieldNew3 ->
        [FieldNew3 | Acc]
    end
      end,

  lists:foldl(F, [], List).

clean_up_record_list_test() ->
  Rec = [{id, 1}, {payment_method, [<<"gw_netbank">>]}, {status, <<"normal">>}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, <<"2016-12-25T21:25:04.012503 +08:00">>}, {date, <<"2016/11/12">>},
    {mcht_full_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183, 229, 133, 172, 229, 143, 184>>},
    {mcht_short_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183>>}, {mcht_full_name, <<>>}],
  RecExpected = [{id, 1}, {payment_method, [gw_netbank]}, {status, normal}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, {1482, 672304, 12503}},
    {mcht_full_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183, 229, 133, 172, 229, 143, 184>>},
    {mcht_short_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183>>}, {mcht_full_name, <<>>}],

  ?assertEqual(RecExpected, lists:reverse(clean_up_record_list(pg_repos_t_repo, Rec))).

kv_clean_record_only({K, V}, Fields) when is_list(Fields) ->
  case lists:member(K, Fields) of
    true ->
      {K, V};
    false ->
      []
  end;
kv_clean_record_only([], _) ->
  [].

kv_clean_convert_atom({K, V}, AtomKeyList) when is_list(AtomKeyList) ->
  VNew = case lists:member(K, AtomKeyList) of
           true ->
             %% convert atom from binary
%%             lager:debug("Convert atoms = ~p", [V]),
             VAtom = convert_atoms(V),
             VAtom;
           false ->
             %% keep
             V
         end,
  {K, VNew};
kv_clean_convert_atom(KV, _) ->
  KV.

kv_clean_convert_atom_test() ->
  Rec = [{payment_method, [<<"gw_netbank">>]}, {status, <<"normal">>}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, <<"2016-12-25T21:25:04.012503 +08:00">>}],
  RecExpected = [{payment_method, [gw_netbank]}, {status, normal}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, <<"2016-12-25T21:25:04.012503 +08:00">>}],
  Seq = lists:seq(1, length(Rec)),
  [?assertEqual(lists:nth(I, RecExpected),
    kv_clean_convert_atom(lists:nth(I, Rec), [payment_method, status]))
    || I <- Seq
  ],
  ok.

convert_atoms([]) ->
  [];
convert_atoms([H | Rest]) ->
  [convert_atoms(H) | convert_atoms(Rest)];
convert_atoms(String) when is_binary(String) ->
  binary_to_existing_atom(String, utf8);
convert_atoms(Atom) when is_atom(Atom) ->
  Atom.


convert_atom_test() ->
  ?assertEqual([gw_netbank], convert_atoms([<<"gw_netbank">>])),
  ?assertEqual([gw_netbank_only], convert_atoms([<<"gw_netbank_only">>])),
  ?assertEqual([gw_netbank], convert_atoms([gw_netbank])),
  ?assertEqual([gw_netbank, gw_wap], convert_atoms([<<"gw_netbank">>, <<"gw_wap">>])).



kv_clean_convert_ts({K, V}, TSKeyList) when is_list(TSKeyList), is_binary(V) ->
  VNew = case lists:member(K, TSKeyList) of
           true ->
             %% convert
             xfutils:datetime_string_to_timestamp(binary_to_list(V));
           false ->
             V
         end,
  {K, VNew};
kv_clean_convert_ts(KV, _) ->
  KV.

kv_clean_convert_ts_test() ->
  TS = <<"2016-12-25T21:25:04.012503 +08:00">>,
  ?assertEqual({update_ts, {1482, 672304, 12503}},
    kv_clean_convert_ts({update_ts, TS}, [update_ts, last_login_ts])),
  ok.


%%-------------------------------------------------------------------
clean_up_value_list_keys(M, ValueList, Keys) when is_atom(M), is_list(ValueList), is_list(Keys) ->
  ValueListClean = clean_up_record_list(M, ValueList),
  F = fun(Key, AccIn) ->
    VLTrimedIn = proplists:delete(Key, AccIn),
    VLTrimedIn
      end,

  VLTrimed = lists:foldl(F, ValueListClean, Keys),
  VLTrimed.

clean_up_value_list_pk(M, ValueList) when is_atom(M), is_list(ValueList) ->
  PkName = table_config_pk(M),
  clean_up_value_list_keys(M, ValueList, [PkName]).


%%-------------------------------------------------------------------
drop_table(M) when is_atom(M) ->
  TableName = pg_repos:table_name(M),
  mnesia:delete_table(TableName).
%%-------------------------------------------------------------------
init(M) ->
  %% table init
  TableName = pg_repos:table_name(M),
  Fields = pg_repos:fields(M),

  TableResidentOption = table_resident_option(M),

  {atomic, ok} = mnesia:create_table(
    TableName,
    [
      {attributes, Fields},
      TableResidentOption
%%      {disc_copies, [node()]}
    ]
  ),


  recreate_index(M),

  %% data init
  InitData = table_config(data_init, M),
  F = fun({PKV, AttrList}) ->
    {ok, Repo} = create(M, PKV, AttrList),
    ok = save(Repo)
      end,
  lists:map(F, InitData),
  ok.

drop_index(M) when is_atom(M) ->
  %% index init
  TableName = pg_repos:table_name(M),
  Indexs = table_config(table_indexes, M),
  [idx_drop(TableName, FieldName) || FieldName <- Indexs],
  ok.

create_index(M) when is_atom(M) ->
  %% index init
  TableName = pg_repos:table_name(M),
  Indexs = table_config(table_indexes, M),
  [idx_init(TableName, FieldName) || FieldName <- Indexs],
  ok.

recreate_index(M) when is_atom(M) ->
  try
    drop_index(M)
  catch
    _:_ ->
      ok
  end,
  create_index(M),
  ok.

idx_init(Table, FieldName) when is_atom(Table), is_atom(FieldName) ->
  {atomic, ok} = mnesia:add_table_index(Table, FieldName).

idx_drop(Table, FieldName) when is_atom(Table), is_atom(FieldName) ->
  {atomic, ok} = mnesia:del_table_index(Table, FieldName).

data_init() ->
  ok.

table_resident_option(M) when is_atom(M) ->
  Option = table_config(table_resident_place, M),
  PlaceOption = case Option of
                  undefined ->
                    disc_copies;
                  disc_copies ->
                    Option;
                  disc_only_copies ->
                    Option
                end,
  {PlaceOption, [node()]}.


%%-------------------------------------------------------------------
%% CRUD

%% create with index name, pk must be integer sequence
create(M, {IndexName, IndexValue}, AttributesMap)
  when is_map(AttributesMap) ->
  List = maps:to_list(AttributesMap),
  create(M, {IndexName, IndexValue}, List);

create(M, {IndexName, IndexValue}, Attributes)
  when is_atom(M), is_atom(IndexName), is_list(Attributes) ->
  IsExisted = is_index_value_exist(M, IndexName, IndexValue),
  do_create(M, IsExisted, {IndexName, IndexValue}, Attributes).

do_create(M, false, {IndexName, IndexValue}, Attributes) ->
  %% check if talbe's pk is sequence , is not , return []
  NewId = next_pk_sequence_id(M),
  PKAndName = {id, NewId},
  AttributesWithoutId = proplists:delete(id, Attributes),
  AttributesWithoutIndexName = proplists:delete(IndexName, AttributesWithoutId),
  %% attach PKs with Attributes
  ValueList = [PKAndName | AttributesWithoutIndexName] ++ [{IndexName, IndexValue}],
  ValueListClean = clean_up_record_list(M, ValueList),
  lager:debug("ValueList for new record = ~p", [ValueListClean]),
  Repo = pg_repos:new(M, ValueListClean),
  ok = save(Repo),
  {ok, Repo};

do_create(_M, true, {IndexName, IndexValue}, _Attributes) ->
  %% pk already exist, duplicated
  lager:error("Create failed, duplicated index ~p,value = ~p", [IndexName, IndexValue]),
  ErrorValueString = error_string(IndexName, IndexValue),
  ErrorValueStringBin = list_to_binary(ErrorValueString),
  IndexNameBin = atom_to_binary(IndexName, utf8),
  ErrorMsgBin = <<"[", IndexNameBin/binary, "]字段取值["/utf8, ErrorValueStringBin/binary, "]重复."/utf8>>,
%%  RetBody = #{error_code => error, error_msg => io_lib:format("~ts", [<<"内部错误"/utf8>>])},
  throw({duplicate_index_value, ErrorMsgBin}).
%%    io_lib:format("[~p]字段取值[~ts]重复", [IndexName, error_string(IndexName, IndexValue)])}).


%%-------------------------------------------------------------------
%% create with pk
create_pk(M, PkValue, AttributesMap)
  when is_map(AttributesMap) ->
  List = maps:to_list(AttributesMap),
  create_pk(M, PkValue, List);

create_pk(M, PkValue, Attributes)
  when is_atom(M), is_list(Attributes) ->
  IsExisted = is_pk_value_exist(M, PkValue),
  do_create_pk(M, IsExisted, PkValue, Attributes).


do_create_pk(M, false, PkValue, Attributes) ->
  PkName = table_config_pk(M),
  VL = [{PkName, PkValue} | Attributes],
  VLClean = clean_up_record_list(M, VL),
  lager:debug("ValueList form new record = ~p", [VLClean]),
  Repo = pg_repos:new(M, VLClean),
  ok = save(Repo),
  {ok, Repo};

do_create_pk(M, true, PkValue, _Attributes) ->
  %% pk already exist, duplicated
  PkName = table_config_pk(M),
  do_create(undefined, true, {PkName, PkValue}, undefined).



error_string(IndexName, Value)
  when (IndexName =:= mcht_full_name) or
  (IndexName =:= mcht_short_name) ->
  io_lib:format("~ts", [Value]);
error_string(IndexName, ValueTuple)
  when is_tuple(ValueTuple), (IndexName =:= mcht_index_key) or
  (IndexName =:= up_index_key) ->
  TupleContent = error_string(IndexName, tuple_to_list(ValueTuple)),
  io_lib:format("{~ts}", [TupleContent]);
error_string(IndexName, ValueList)
  when is_list(ValueList) ->
  ListContent = [error_string(IndexName, X) ++ "," || X <- ValueList],
  io_lib:format("[~ts]", [ListContent]);
error_string(_, Value) ->
  io_lib:format("~p", [Value]).

error_string_test() ->

  MchtIndexKey = {<<"000012">>, <<"20161212">>, <<"yyyy33388">>},
  ?assertEqual(lists:flatten(error_string(mcht_index_key, MchtIndexKey)), "{[<<\"000012\">>,<<\"20161212\">>,<<\"yyyy33388\">>,]}"),

  MchtFullName = <<"徐峰"/utf8>>,
  ?assertEqual(lists:flatten(error_string(mcht_full_name, MchtFullName)), [24464, 23792]),
  ok.

%%-------------------------------------------------------------------
%% read with pk
%% pk may be binary, need to translate to integer
convert_pk(_M, PKValue, integer) when is_binary(PKValue) ->
  binary_to_integer(PKValue);
convert_pk(_M, PKValue, integer) when is_integer(PKValue) ->
  PKValue;
convert_pk(_M, PKValue, _) ->
  PKValue.


fetch_model(M, PK) when is_atom(M) ->
  {ok, Repo} = fetch(M, PK),
  {ok, pg_repos:to_model(M, Repo)}.

fetch(M, PK) when is_atom(M) ->
  Repo = read(M, PK),
  {ok, Repo}.

read(M, PKValue) when is_atom(M) ->
  TableName = pg_repos:table_name(M),
  PKType = table_config(pk_type, M),
  RealPKValue = convert_pk(M, PKValue, PKType),
  Repo = mnesia:dirty_read(TableName, RealPKValue),
  Repo.

fetch_index_model(M, KV) when is_atom(M), is_tuple(KV) ->
  {ok, Repo} = fetch_index(M, KV),
  {ok, pg_repos:to_model(M, Repo)}.

fetch_index(M, KV) when is_atom(M), is_tuple(KV) ->
  {ok, read_index(M, KV)}.

%% read with index
read_index(M, {IndexName, IndexValue}) when is_atom(M), is_atom(IndexName) ->
  read_index(M, IndexName, IndexValue).

read_index(M, IndexName, IndexValue) when is_atom(M), is_atom(IndexName) ->
  TableName = pg_repos:table_name(M),
  Result = mnesia:dirty_index_read(TableName, IndexValue, IndexName),
  Result.


%%-------------------------------------------------------------------
%% fetch_by/3 fetch_by_index/3
%% return only one or some field of repo
-spec fetch_by(M, PKValue, Key) -> Return when
  M :: atom(),
  PKValue :: any(),
  Key :: atom() | [atom()],
  Return :: any().


fetch_by(M, PKValue, Key) when is_atom(M), is_atom(Key) ->
  case fetch(M, PKValue) of
    {ok, []} ->
      not_found;
    {ok, [Repo]} ->
      pg_repos:get(M, Repo, Key)
  end.

-spec fetch_by_index(M, IndexValue, Key) -> Return when
  M :: atom(),
  IndexValue :: any(),
  Key :: atom() | [atom()],
  Return :: any().

fetch_by_index(M, IndexValue, Key) when is_atom(M), is_atom(Key) ->
  case fetch_index(M, IndexValue) of
    {ok, []} ->
      not_found;
    {ok, [Repo]} ->
      pg_repos:get(M, Repo, Key)
  end.
%%-------------------------------------------------------------------
%% update with pk
update_pk(M, PkValue, ValueList)
  when is_atom(M), is_list(ValueList) ->
  update_pk(M, PkValue, ValueList, update).

update_pk(M, PkValue, ValueList, Option)
  when is_atom(M), is_list(ValueList), is_atom(Option) ->
  Repo = read(M, PkValue),
  ValueListWithoutPk = clean_up_value_list_pk(M, ValueList),
  do_update_pk(M, Repo, PkValue, ValueListWithoutPk, Option).

do_update_pk(_, [], _, _, update) ->
  %% not found, update return ok
  {ok, []};
do_update_pk(M, [], PkValue, ValueList, update_create) ->
  %% not found, need create
  {ok, Repo} = create_pk(M, PkValue, ValueList),
  {ok, Repo};
do_update_pk(M, [Repo], _, ValueList, _) when is_atom(M), is_tuple(Repo), is_list(ValueList) ->
  %% found, need update
  %% VL already cleaned
  RepoNew = pg_repos:set(M, Repo, ValueList),
  lager:debug("update with Repo = ~p", [RepoNew]),
  ok = save(RepoNew),
  {ok, RepoNew}.

%% update_create
update_create_pk(M, PkValue, ValueList)
  when is_atom(M), is_list(ValueList) ->
  update_pk(M, PkValue, ValueList, update_create).
%%-------------------------------------------------------------------
%% update with index
update(M, {IndexName, IndexValue}, ValueList)
  when is_atom(M), is_atom(IndexName), is_list(ValueList) ->
  update(M, {IndexName, IndexValue}, ValueList, update).

update(M, {IndexName, IndexValue}, ValueList, Option)
  when is_atom(M), is_atom(IndexName), is_list(ValueList)
  , is_atom(Option) ->

  %% if pk exist , and is not zero,in ValueList ,update with pk
  IsPkExist = check_pk_exists(M, ValueList),
  %% if pk not exist in ValueList , update with index
  Repo = case IsPkExist of
           {true, PkValue} ->
             %% fetch with pk
             read(M, PkValue);
           false ->
             read_index(M, {IndexName, IndexValue})
         end,
  %% delete pk from ValueList, in case of pass {id,0}
%%  PkName = table_config(pk_key_name, M),
  ValueListWithoutPk = clean_up_value_list_pk(M, ValueList),
  do_update(M, Repo, {IndexName, IndexValue}, [{IndexName, IndexValue} | ValueListWithoutPk], Option).

do_update(_, [], _, _, update) ->
  %% not found , update return ok
  {ok, []};
do_update(M, [], {IndexName, IndexValue}, ValueList, update_create) ->
  %% not found , need create
  {ok, Repo} = create(M, {IndexName, IndexValue}, ValueList),
  %% ok = save(Repo),
  {ok, Repo};
do_update(M, [Repo], _, ValueList, _) when is_atom(M), is_tuple(Repo), is_list(ValueList) ->
  %% found, need update
  CleanedValueList = clean_up_record_list(M, ValueList),
  RepoNew = pg_repos:set(M, Repo, CleanedValueList),
  lager:debug("update with repo = ~p", [RepoNew]),
  ok = save(RepoNew),
  {ok, RepoNew}.

check_pk_exists(M, ValueList) when is_atom(M), is_list(ValueList) ->
  PkName = table_config(pk_key_name, M),
  PkType = table_config(pk_type, M),
  PkValue = proplists:get_value(PkName, ValueList),
  case PkValue of
    0 ->
      false;
    <<"0">> ->
      false;
    undefined ->
      false;
    PkValue ->
      PkValueInteger = convert_pk_value(PkValue, PkType),
      {true, PkValueInteger}
  end.

convert_pk_value(Value, integer) when is_binary(Value) ->
  binary_to_integer(Value);
convert_pk_value(Value, _) ->
  Value.

%% update_create
update_create(M, {IndexName, IndexValue}, ValueList)
  when is_atom(M), is_atom(IndexName), is_list(ValueList) ->
  update(M, {IndexName, IndexValue}, ValueList, update_create).


%%-------------------------------------------------------------------
%% get all items from repo
get_all(M) when is_atom(M) ->
  IdList = all_pks(M),
  F = fun(ID, Acc) ->
    [Repo] = read(M, ID),
    [Repo | Acc]
      end,
  lists:foldl(F, [], IdList).

%%-------------------------------------------------------------------
qh_table(M) when is_atom(M) ->
  TableName = pg_repos:table_name(M),
  qlc:q([X || X <- mnesia:table(TableName)]).
%%-------------------------------------------------------------------
qh_probe(X) ->
  case xfutils:get(trace_repo_probe) of
    true ->
      lager:debug("probe X = ~p", [X]);
    _ ->
      ok
  end,
  true.
%%-------------------------------------------------------------------
qh_one_cond(M, {tuple, Pos, Key, Value} = Cond, PrevQH) when is_atom(M), is_tuple(Cond), is_atom(Key), is_integer(Pos) ->
  %% check Key must be a field of record
  true = lists:member(Key, pg_repos:fields(M)),
%%  QueryOption = apply(M, query_option, [Key]),

  F = fun
        (Value, Tuple) ->
          element(Pos, Tuple) =:= Value
      end,

  QH = qlc:q([X || X <- PrevQH
%%    , qh_probe(X)
    , F(Value, pg_repos:get(M, X, Key))
  ]),
  QH;
qh_one_cond(M, {in, Key, List} = Cond, PrevQH) when is_atom(M), is_tuple(Cond), is_atom(Key), is_list(List) ->
  %% check key vlaue must with a value list
  true = lists:member(Key, pg_repos:fields(M)),
%%  QueryOption = apply(M, query_option, [Key]),

  F = fun
        (List, [KeyValue]) ->
          lists:member(KeyValue, List);
        (List, KeyValue) ->
          lists:member(KeyValue, List)
      end,

%%  QH = qlc:q([X || X <- PrevQH
%%%%    , qh_probe(X)
%%    , F(List, pg_repos:get(M, X, Key))
%%  ]),
  QH = do_qh_one_cond(M, Key, PrevQH, F, [List]),
  QH;
qh_one_cond(M, {between, Key, Start, End} = Cond, PrevQH)
  when is_atom(M), is_tuple(Cond), is_atom(Key) ->
  %% check key vlaue must with start/end value
  true = lists:member(Key, pg_repos:fields(M)),
%%  QueryOption = apply(M, query_option, [Key]),

  F = fun
        (Start, End, KeyValue) ->
          (Start =< KeyValue) and (KeyValue =< End)
      end,

%%  QH = qlc:q([X || X <- PrevQH
%%%%    , qh_probe(X)
%%    , F(Start, End, pg_repos:get(M, X, Key))
%%  ]),
  QH = do_qh_one_cond(M, Key, PrevQH, F, [Start, End]),
  QH;
qh_one_cond(M, {Key, Value} = Cond, PrevQH) when is_atom(M), is_tuple(Cond), is_atom(Key) ->
  %% check Key must be a field of record
  true = lists:member(Key, pg_repos:fields(M)),
%%  QueryOption = apply(M, query_option, [Key]),
  QueryOptionConfig = table_config(query_option, M),
  QueryOption = maps:get(Key, QueryOptionConfig, full_match),
  F = fun_key_value_compare(QueryOption),
%%  QH = qlc:q([X || X <- PrevQH,
%%%%    qh_probe(X),
%%    F(Value, pg_repos:get(M, X, Key))
%%  ]),
  QH = do_qh_one_cond(M, Key, PrevQH, F, [Value]),
  QH.


%%do_qh_one_cond(M, Key, PrevQH, FCompare, Values)
do_qh_one_cond(M, Key, PrevQH, FCompare, [ValueInPost])
  when is_atom(Key), is_atom(M), is_function(FCompare)
%%  , is_list(ValueInPost)
  ->
  QH = qlc:q([X || X <- PrevQH,
    qh_probe(X),
%%    apply(FCompare, Values ++ [pg_repos:get(M, X, Key)])
    FCompare(ValueInPost, pg_repos:get(M, X, Key))
  ]),
  QH;
do_qh_one_cond(M, Key, PrevQH, FCompare, [ValueInPost1, ValueInPost2])
  when is_atom(Key), is_atom(M), is_function(FCompare)
%%  , is_list(ValueInPost)
  ->
  QH = qlc:q([X || X <- PrevQH,
    qh_probe(X),
%%    apply(FCompare, Values ++ [pg_repos:get(M, X, Key)])
    FCompare(ValueInPost1, ValueInPost2, pg_repos:get(M, X, Key))
  ]),
  QH.

%%-------------------------------------------------------------------
parse_int(Value) when is_binary(Value) ->
  binary_to_integer(Value);
parse_int(Value) when is_integer(Value) ->
  Value.


fun_key_value_compare(integer_equal) ->
  F = fun
        (ValuePosted, ValueInRepo) ->
          IntValuePosted = parse_int(ValuePosted),
          IntValueInRepo = parse_int(ValueInRepo),

          IntValueInRepo =:= IntValuePosted
      end,
  F;
fun_key_value_compare(full_match) ->
  F = fun
        (ValuePosted, ValueInRepo) ->
%%          lager:debug("ValuePosted = ~p,ValueInRepo = ~p", [ValuePosted, ValueInRepo]),
          ValuePosted =:= ValueInRepo
      end,
  F;
fun_key_value_compare(within) ->
  F = fun
        (ValuePosted, ValueInRepo) ->
          binary:match(ValueInRepo, ValuePosted) =/= nomatch
      end,
  F;
fun_key_value_compare(member) ->
  F = fun
        (ValuePosted, ValueInRepo) ->
          lists:member(ValuePosted, ValueInRepo)
      end,
  F.

fun_key_value_compare_test() ->
  F_match = fun_key_value_compare(full_match),
  ?assertEqual(F_match(1, 1), true),
  ?assertEqual(F_match(<<"1">>, <<"1">>), true),
  ?assertEqual(F_match(<<"123">>, <<"12">>), false),


  F_member = fun_key_value_compare(member),
  ?assertEqual(F_member(1, [1, 2, 3]), true),
  ?assertEqual(F_member([1], [1, 2, 3]), false),

  F_within = fun_key_value_compare(within),
  ?assertEqual(F_within(<<"123">>, <<"12345">>), true),
  ?assertEqual(F_within(<<"12345">>, <<"123456">>), true),
  ?assertEqual(F_within(<<"12345">>, <<"123">>), false),

  ok.

%%-------------------------------------------------------------------
query(M, Critia, Keys) when is_atom(M), is_list(Critia), is_list(Keys) ->
  L = query(M, Critia),
  L1 = [pg_repos:get(M, Repo, Keys) || Repo <- L],
  L1.

query(M, Critia) when is_atom(M), is_list(Critia) ->
  QH = qh(M, Critia),
  L = e(QH),
  L.

qh(M, Critia) when is_atom(M), is_list(Critia) ->
  %% build qlc
  FilterList = proplists:get_value(filter, Critia),

  F = fun(Cond, PrevQH) ->
    QHNew = qh_one_cond(M, Cond, PrevQH),
    QHNew
      end,


  QHFinal = lists:foldl(F, qh_table(M), FilterList),
  %%QHFinal = F({name,}, qh_table(M)),
  QHFinal.

%%  L = e(QHFinal),
%%
%%  L.

%%-------------------------------------------------------------------
%%-------------------------------------------------------------------
e(QH) ->
  F = fun() ->
    qlc:e(QH)
      end,

  {atomic, L} = mnesia:transaction(F),
  L.


%%-------------------------------------------------------------------
convert_id_value({id, Binary}) when is_binary(Binary) ->
  {id, binary_to_integer(Binary)};
convert_id_value(KV) ->
  KV.

bindings_2_filter_cond(Bindings, QueryIndexes) when is_list(Bindings), is_list(QueryIndexes) ->
  %% if one indexkey value = all or 0, then omit this indexkey
  F = fun
        ({IndexKey, BindVariable}, Acc) ->
          IndexValue = proplists:get_value(BindVariable, Bindings),
          NewAcc = case IndexValue of
                     0 ->
                       Acc;
                     all ->
                       Acc;
                     <<"all">> ->
                       Acc;
                     _ ->
                       KV = {IndexKey, proplists:get_value(BindVariable, Bindings)},
                       KVNew = convert_id_value(KV),
                       [KVNew | Acc]

                   end,
          NewAcc
      end,

  L = lists:foldl(F, [], QueryIndexes),
  lists:reverse(L).
%%  [{IndexKey, proplists:get_value(BindVariable, Bindings)} || {IndexKey, BindVariable} <- QueryIndexes].

bindings_2_filter_cond_test() ->
  Bindings = [{id, <<"1">>}, {mchtId, 13}, {txnDate, <<"20160101">>}, {other1, <<"all">>}, {other2, 0}, {other3, all}],
  QueryIndexes = [{id, id}, {mcht_id, mchtId}, {mcht_txn_date, txnDate}, {other, other1}, {other, other2}, {other, other3}],

  Result = [{id, 1}, {mcht_id, 13}, {mcht_txn_date, <<"20160101">>}],

  ?assertEqual(Result, bindings_2_filter_cond(Bindings, QueryIndexes)),
  ok.


%%-------------------------------------------------------------------
%% migration related
%% dump to proplist, then to file
dump_all_to_file(FileName) when is_list(FileName) ->
  MList = [repo_backend_users_pt, repo_mchants_pt, repo_mcht_txn_log_pt, repo_up_txn_log_pt],
  [dump_to_file(M, FileName) || M <- MList].


dump_to_file(M, FileName) when is_atom(M), is_binary(FileName) ->
  dump_to_file(M, binary_to_list(FileName));
dump_to_file(M, FileName) when is_atom(M), is_list(FileName) ->
  L = get_all(M),
  LProps = [maps:to_list(pg_repos:to_model(M, X)) || X <- L],
  LTerm = [io_lib:format("~tp.~n", [X]) || X <- LProps],
  file:write_file(FileName, LTerm, [append]),
  lager:debug("Dump talbe ~p to file with proplists end! ", [M]),
  ok.

%%-------------------------------------------------------------------
dump_all_to_file_safe(FileName) when is_list(FileName) ->
  MList = [repo_backend_users_pt, repo_mchants_pt, repo_mcht_txn_log_pt, repo_up_txn_log_pt],
  [dump_to_file_safe(M, FileName) || M <- MList].

repo_2_term(M, Repo) ->
  Props = maps:to_list(pg_repos:to_model(M, Repo)),
  Term = io_lib:format("~tp.~n", [Props]),
  Term.


dump_to_file_safe(M, FileName) when is_atom(M), is_binary(FileName) ->
  dump_to_file_safe(M, binary_to_list(FileName));
dump_to_file_safe(M, FileName) when is_atom(M), is_list(FileName) ->
  erlang:garbage_collect(self()),
  L = get_all(M),
  %% truncate file
  file:write_file(FileName, [], [write]),

  %% dump every N lines
  LinesGap = 500,
  F = fun
        (Repo, {N, Acc, Total}) when N >= LinesGap ->
          %% reach write threshold
          %% dump this to file
          lager:info("Write ~p lines to file:~ts", [Total, FileName]),
          file:write_file(FileName, Acc, [append]),
          %% initial new empty acc
          {1, [repo_2_term(M, Repo)], Total + N};
        (Repo, {N, Acc, Total}) ->
          {N + 1, [repo_2_term(M, Repo) | Acc], Total}
      end,

  {N, Rest, SubTotal} = lists:foldl(F, {0, [], 0}, L),

  %% dump rest
  lager:info("Write ~p lines to file:~ts", [SubTotal + N, FileName]),
  file:write_file(FileName, Rest, [append]),

  lager:debug("Dump talbe ~p to file with proplists end! ", [M]),

  os:cmd("gzip " ++ FileName),
  lager:debug("Compress ~p end! ", [FileName]),
  ok.

%%-------------------------------------------------------------------
dump_to_file_safe_fold(M, FileName) when is_atom(M), is_binary(FileName) ->
  dump_to_file_safe_fold(M, binary_to_list(FileName));
dump_to_file_safe_fold(M, FileName) when is_atom(M), is_list(FileName) ->
  QH = qh(M, [{filter, []}]),
  %% truncate file
  file:write_file(FileName, [], [write]),

  %% dump every N lines
  LinesGap = 500,
  F = fun
        (Repo, {N, Acc, Total}) when N >= LinesGap ->
          %% reach write threshold
          %% dump this to file
          lager:info("Write ~p lines to file:~ts", [Total, FileName]),
          file:write_file(FileName, Acc, [append]),
          %% initial new empty acc
          {1, [repo_2_term(M, Repo)], Total + N};
        (Repo, {N, Acc, Total}) ->
          {N + 1, [repo_2_term(M, Repo) | Acc], Total}
      end,

  F1 = fun() ->
    qlc:fold(F, {0, [], 0}, QH)
       end,
  {atomic, {N, Rest, SubTotal}} = mnesia:transaction(F1),

  %% dump rest
  lager:info("Write ~p lines to file:~ts", [SubTotal + N, FileName]),
  file:write_file(FileName, Rest, [append]),

  lager:info("Dump talbe ~p to file with proplists end! ", [M]),

  os:cmd("gzip " ++ FileName),
  lager:info("Compress ~p end! ", [FileName]),
  ok.
%%-------------------------------------------------------------------
%% load from proplist
load_from_file(M, FileName) when is_atom(M), is_list(FileName) ->
  {ok, L} = file:consult(FileName),
  F = fun(Repo, Acc) ->
    save(M, maps:from_list(Repo), [dirty])
      end,
  lists:foldl(F, [], L),
%%  [save(M, maps:from_list(X)) || X <- L],
  lager:debug("Load table ~p from file with proplists end!", [M]),
  ok.

%% load all backup from backup.db dir
-define(APP, payment_gateway).
backup_db_file_name(M, Date) when is_atom(M), is_binary(Date) ->
  %% todo: not hard code application name:payment_gateway here
  DbBackupPath = xfutils:get_path(?APP, [home, db_backup_dir]),

  DbBackupFileName = DbBackupPath
    ++ "mnesia.backup."
    ++ atom_to_list(M)
    ++ "."
    ++ binary_to_list(Date),
  DbBackupFileName.

load_all_from_file(Date) when is_binary(Date) ->
  Tables = [mchants, mcht_txn_log, ums_reconcile_result, up_txn_log],
  Repos = lists:foldl(
    fun(Tbl, Acc) ->
      TblBinary = atom_to_binary(Tbl, utf8),
      RepoBinary = <<"repo_", TblBinary/binary, "_pt">>,
      Repo = binary_to_existing_atom(RepoBinary, utf8),
      [Repo | Acc]
    end,
    [],
    Tables
  ),
  lager:info("Loading from Repos = ~p", [Repos]),

  F = fun(Tbl) ->
    TblBinary = atom_to_binary(Tbl, utf8),
    RepoBinary = <<"repo_", TblBinary/binary, "_pt">>,
    Repo = binary_to_existing_atom(RepoBinary, utf8),

    DbFileName = backup_db_file_name(Repo, Date),

    lager:info("Load ~p from file ~p", [Repo, DbFileName]),

    load_from_file(Repo, DbFileName)
      end,

  [F(Tbl) || Tbl <- Tables].


%%-------------------------------------------------------------------
%% trnasfer txn log from txn log to history log
transfer_history_log(From, To) when is_atom(From), is_atom(To) ->
  transfer_history_log(From, To, []).

transfer_history_log(From, To, Options) when is_atom(From), is_atom(To), is_list(Options) ->
  StartDate = get_transfer_option(transfer_start_date, Options, default_transfer_date_start()),
  EndDate = get_transfer_option(transfer_end_date, Options, default_transfer_date_end()),
  DateField = get_transfer_option(date_field, Options, undefined),
  DeleteOld = get_transfer_option(delete_old, Options, true),

  lager:debug("Transfer history log, StartDate = ~p,EndDate = ~p,DateField = ~p",
    [StartDate, EndDate, DateField]),
  QH = qh(From, [{filter, [{between, DateField, StartDate, EndDate}]}]),

  FTransfer = fun
                (FromTxnRepo, Acc) ->
                  SourceMap = pg_repos:to_map(From, FromTxnRepo),
                  ToTxnRepo = pg_repos:from_map(To, SourceMap),
                  ok = save(ToTxnRepo),
                  case DeleteOld of
                    true ->
                      delete(FromTxnRepo);
                    _ ->
                      ok
                  end,
                  Acc + 1

              end,
  F = fun
        () ->
          qlc:fold(FTransfer, 0, QH)
      end,
  {atomic, TotalTransferedLogs} = mnesia:transaction(F),
  lager:info("Transfer ~p lines success...", [TotalTransferedLogs]),
  ok.

get_transfer_option(Key, Options, DefaultValue) ->
  proplists:get_value(Key, Options, DefaultValue).

default_transfer_date_start() ->
  Yesterday = datetime_x_fin:yesterday(),
  default_transfer_date_start(Yesterday).

default_transfer_date_start(Date) when is_binary(Date) ->
  Gap = 3 * 30 + 1,
  {date_yyyymmdd, DateBeforeGap} = datetime_x_fin:dec_days({date_yyyymmdd, Date}, Gap),
  DateBeforeGap.

default_transfer_date_end() ->
  Yesterday = datetime_x_fin:yesterday(),
  default_transfer_date_end(Yesterday).

default_transfer_date_end(Date) when is_binary(Date) ->
  Gap = 3 * 30,
  {date_yyyymmdd, DateBeforeGap} = datetime_x_fin:dec_days({date_yyyymmdd, Date}, Gap),
  DateBeforeGap.

default_transfer_date_start_test() ->
  ?assertEqual(<<"20170328">>, default_transfer_date_start(<<"20170627">>)),
  ok.




