-module(pg_repos).
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([
  table_name/1
  , fields/1
  , new/2
  , new_empty/1
  , get/3
  , get/4
  , get_in/3
  , get_in/4
  , set/3
  , set/4
  , inc/3
  , inc/4
  , convert/3

  , from_map/2
  , from_model/2
%%  , to_model/2
%%  , to_map/2
%%  , to_proplists/2
  , pr/2
  , lager/3

  , next_id/1
  , next_id/2
]).

%%-compile(export_all).

-define(TEST_REPO, pg_repos_t_repo).
-define(TEST_REPO_TBL, mchants).
-define(TEST_MODEL, pg_repos_t_model).

%%====================================================================
%% API functions
%%====================================================================
%% exprecs interface
table_name(M) when is_atom(M) ->
  [TableName] = M: '#exported_records-'(),
  TableName.

table_name_test() ->
  ?assertEqual(?TEST_REPO_TBL, table_name(?TEST_REPO)),
  ?assertEqual(?TEST_MODEL, table_name(?TEST_MODEL)),
  ok.
%%-----------------------------------------------------------------
fields(M) when is_atom(M) ->
  TableName = table_name(M),
  Fields = M: '#info-'(TableName, fields),
  Fields.

fields_test() ->
  ?assertEqual([id, mcht_full_name, mcht_short_name, status
    , payment_method, up_mcht_id, quota, up_term_no, update_ts]
    , fields(?TEST_MODEL)),
  ok.
%%-----------------------------------------------------------------
new_empty(M) when is_atom(M) ->
  TableName = table_name(M),
  M: '#new-'(TableName).

new_empty_test() ->
  A = new_empty(?TEST_REPO),
  TS = get_in(?TEST_REPO, A, update_ts),
  ?assertEqual({?TEST_REPO_TBL, 0, <<"">>, <<"">>, normal, [gw_netbank], <<"">>,
    [{txn, -1}, {daily, -1}, {monthly, -1}], <<"12345678">>, TS}
    , A),
  ok.

%%-----------------------------------------------------------------
new(M, List) when is_atom(M), is_list(List) ->
  EmptyRec = new_empty(M),
  NewRec = M: '#set-'(List, EmptyRec),
  NewRec;

new(M, Map) when is_atom(M), is_map(Map) ->
  List = maps:to_list(Map),
  new(M, List).

new_test() ->
  ?assertEqual(new(?TEST_REPO, [{id, 1}, {mcht_full_name, <<"aaa">>}, {update_ts, <<>>}])
    , new(?TEST_REPO, #{id=>1, mcht_full_name=><<"aaa">>, update_ts => <<>>})),
  ok.

%%-------------------------------------------------------------------
%% getter/setter
get_in(M, Repo, Key, Default) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  case get_in(M, Repo, Key) of
    undefined -> Default;
    Value -> Value
  end.

get_in(M, Repo, Key) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
%%  Value = apply(M, '#get-', [Key, Repo]),
  Value = M: '#get-'(Key, Repo),
  Value;
get_in(M, Repo, Keys) when is_atom(M), is_tuple(Repo), is_list(Keys) ->
%%  Values = apply(M, '#get-', [Keys, Repo]),
%%  Values.
  [get_in(M, Repo, Key) || Key <- Keys].

get_in_test() ->
  R = new_test(model),
  ?assertEqual(1, get_in(?TEST_MODEL, R, id)),
  ?assertEqual(<<"full">>, get_in(?TEST_MODEL, R, mcht_full_name)),

  ?assertEqual([1, <<"full">>], get_in(?TEST_MODEL, R, [id, mcht_full_name])),
  ok.
%%-------------------------------------------------------------------
get(M, Repo, Key) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  M:get(Repo, Key).

get(M, Repo, Key, Default) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  M:get(Repo, Key, Default).

get_test() ->
  R = new_test(model),
  ?assertEqual({1, <<"full">>}, get(?TEST_MODEL, R, aa)),
  ok.
%%-------------------------------------------------------------------
set(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
set(M, Repo, Key, Value) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  ValueList = [{Key, Value}],
  set(M, Repo, ValueList).

set(M, Repo, ValueLists) when is_atom(M), is_tuple(Repo), is_list(ValueLists) ->
  %%TableName = table_name(M),
%%  RepoNew = apply(M, '#set-', [ValueLists, Repo]),
  RepoNew = M: '#set-'(ValueLists, Repo),
  RepoNew.

set_test() ->
  R = new_test(model),
  ?assertEqual({error, pk_could_not_be_changed}, set(?TEST_MODEL, R, id, 333)),
  R1 = set(?TEST_MODEL, R, mcht_full_name, <<"new full">>),
  ?assertEqual(<<"new full">>, get(?TEST_MODEL, R1, mcht_full_name)),

  R2 = set(?TEST_MODEL, R, [{mcht_full_name, <<"new full">>}, {update_ts, 200}]),
  ?assertEqual(<<"new full">>, get(?TEST_MODEL, R2, mcht_full_name)),
  ?assertEqual(200, get(?TEST_MODEL, R2, update_ts)),

  ok.
%%-------------------------------------------------------------------
inc(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
inc(M, Repo, Key, IncValue) when is_atom(M), is_tuple(Repo), is_atom(Key), is_integer(IncValue) ->
  OldValue = get(M, Repo, Key),
  ValueList = [{Key, OldValue + IncValue}],
%%  RepoNew = apply(M, '#set-', [ValueList, Repo]),
  RepoNew = M: '#set-'(ValueList, Repo),
  RepoNew.

inc(M, Repo, {Key, IncValue}) when is_atom(M), is_tuple(Repo), is_integer(IncValue), is_atom(Key) ->
  inc(M, Repo, Key, IncValue).

inc_test() ->
  R = new_test(model),
  ?assertEqual({error, pk_could_not_be_changed}, inc(?TEST_MODEL, R, id, 1)),
  ?assertEqual(set(?TEST_MODEL, R, update_ts, 101), inc(?TEST_MODEL, R, update_ts, 1)),
  ?assertEqual(set(?TEST_MODEL, R, update_ts, 102), inc(?TEST_MODEL, R, {update_ts, 2})),
  ok.


%%-------------------------------------------------------------------
convert(M, Repo, proplists) when is_tuple(Repo) ->
  to_proplists(M, Repo);
convert(M, Repo, map) when is_tuple(Repo) ->
  to_map(M, Repo);
convert(M, Repo, model) when is_tuple(Repo) ->
  to_map(M, Repo);
convert(M, Repos, model) when is_list(Repos) ->
  to_model(M, Repos);
convert(M, Repo, poststring) when is_tuple(Repo) ->
  to_post(M, Repo, string).
%%-------------------------------------------------------------------
%% model (map) <==> repo(record)
to_proplists(M, Repo) when is_atom(M), is_tuple(Repo) ->
  Fields = fields(M),
  ValueList = tl(tuple_to_list(Repo)),
%%  lager:info("Fields = ~p,ValueList = ~p", [Fields, ValueList]),
  Ret = lists:zip(Fields, ValueList),
%%  lager:info("Ret = ~p", [Ret]),
  Ret.

to_proplists_test() ->
  R = new_test(model),
  ?assertEqual(
    [
      {id, 1}
      , {mcht_full_name, <<"full">>}
      , {mcht_short_name, <<"short">>}
      , {status, normal}
      , {payment_method, [gw_netbank]}
      , {up_mcht_id, <<>>}
      , {quota, [{txn, -1}, {daily, -1}, {monthly, -1}]}
      , {up_term_no, <<"12345678">>}
      , {update_ts, 100}
    ]
    , to_proplists(?TEST_MODEL, R)),
  ok.

%%-------------------------------------------------------------------
to_map(M, Repo) when is_atom(M), is_tuple(Repo) ->
  List = to_proplists(M, Repo),
  maps:from_list(List).

to_map_test() ->
  R = new_test(model),
  ?assertEqual(
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
    }
    , to_map(?TEST_MODEL, R)),
  ok.

%%-------------------------------------------------------------------
to_post(M, Repo, string) when is_atom(M), is_tuple(Repo) ->
  PL = to_proplists(M, Repo),
  xfutils:post_vals_to_iolist(PL).

to_post_test() ->
  R = new_test(model),
  R1 = set(?TEST_MODEL, R, [{quota, <<>>}, {payment_method, <<>>}]),
  ?assertEqual(
    <<"id=1&mcht_full_name=full&mcht_short_name=short&status=normal&up_term_no=12345678&update_ts=100">>
    , list_to_binary(to_post(?TEST_MODEL, R1, string))
  ),
  ok.

%%-------------------------------------------------------------------

to_model(M, List) when is_atom(M), is_list(List) ->
  [to_model(M, Repo) || Repo <- List];
to_model(M, Repo) when is_atom(M), is_tuple(Repo) ->
  to_map(M, Repo).

to_model_test() ->
  R = new_test(model),
  R1 = inc(?TEST_MODEL, R, update_ts, 1),
  L = convert(?TEST_MODEL, R, proplists),

  ?assertEqual(
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
    }
    , to_model(?TEST_MODEL, R)),
  ?assertEqual(
    [
      #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
        , status=>normal, payment_method =>[gw_netbank]
        , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
        , up_term_no=> <<"12345678">>, update_ts => 100
      }
      ,
      #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
        , status=>normal, payment_method =>[gw_netbank]
        , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
        , up_term_no=> <<"12345678">>, update_ts => 101
      }
    ], to_model(?TEST_MODEL, [R, R1])),
  ok.
%%-------------------------------------------------------------------
from_model(M, Model) when is_atom(M), is_map(Model) ->
  from_map(M, Model).

from_map(M, Model) when is_atom(M), is_map(Model) ->
  List = maps:to_list(Model),
  %%TableName = table_name(M),
  EmptyR = new_empty(M),
%%  apply(M, '#fromlist-', [List, EmptyR]).
  M: '#fromlist-'(List, EmptyR).


from_map_test() ->
  R = new_test(model),
  Model =
    #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>
      , status=>normal, payment_method =>[gw_netbank]
      , up_mcht_id=><<>>, quota=>[{txn, -1}, {daily, -1}, {monthly, -1}]
      , up_term_no=> <<"12345678">>, update_ts => 100
    },
  ?assertEqual(R, from_map(?TEST_MODEL, Model)),
  ?assertEqual(R, from_model(?TEST_MODEL, Model)),
  ok.
%%-------------------------------------------------------------------
pr(M, Repo) when is_atom(M), is_tuple(Repo) ->
  VL = to_proplists(M, Repo),
  L = [pr_field(M, Field, Value) || {Field, Value} <- VL],
  lists:flatten(L);
pr(M, Model) when is_atom(M), is_map(Model) ->
  L = [pr_field(M, Field, maps:get(Field, Model)) || Field <- maps:keys(Model)],
  lists:flatten(L).

pr_field(M, Field, Value) ->
  ValueFormatter = M:field_pr_formatter(Field),
  io_lib:format(ValueFormatter, [Field, Value]).

pr_test() ->
  R = new_test(model),


  Out = pr(?TEST_MODEL, R),
  OutTrim = trim_pretty(Out),
  Expected = <<"id=1,mcht_full_name=full,mcht_short_name=short,status=normal,payment_method=[gw_netbank],up_mcht_id=<<>>,quota=[{txn,-1},{daily,-1},{monthly,-1}],up_term_no=<<\"12345678\">>,update_ts=100,">>,

  [E] = io_lib:format("~ts", [Expected]),
  ?assertEqual(E, OutTrim),

  R1 = set(?TEST_MODEL, R, mcht_full_name, <<"测试"/utf8>>),
  Exp2 = <<"id=1,mcht_full_name=测试,mcht_short_name=short,status=normal,payment_method=[gw_netbank],up_mcht_id=<<>>,quota=[{txn,-1},{daily,-1},{monthly,-1}],up_term_no=<<\"12345678\">>,update_ts=100,"/utf8>>,
  [E2] = io_lib:format("~ts", [Exp2]),
  ?assertEqual(E2, trim_pretty(pr(?TEST_MODEL, R1))),


  ok.

trim_pretty(L) ->
  F = fun(Char, AccIn) when (Char =:= 32) or (Char =:= 10) ->
    AccIn;
    (Char, AccIn) ->
      [Char | AccIn]
      end,

  RL = lists:foldl(F, [], L),
  lists:reverse(RL).


%%-------------------------------------------------------------------
lager(Level, M, Model) ->
  String = pr(M, Model),
  lager_out(Level, M, String).

lager_out(debug, M, String) ->
  lager:debug("~p = ~ts", [M, String]);
lager_out(info, M, String) ->
  lager:info("~p = ~ts", [M, String]);
lager_out(error, M, String) ->
  lager:error("~p = ~ts", [M, String]).
%%-------------------------------------------------------------------
%% find next avail id from list, start from StartFrom, which default for 1

next_id(IdList) when is_list(IdList) ->
  next_id(lists:sort(IdList), 1).


find_least_avail_id(_Id, {stop, IdStart}) ->
  {stop, IdStart};
find_least_avail_id(Id, {continue, IdStart}) when Id < IdStart ->
  {continue, IdStart};
find_least_avail_id(Id, {continue, IdStart}) when Id =:= IdStart ->
  {continue, IdStart + 1};
find_least_avail_id(Id, {continue, IdStart}) when Id > IdStart ->
  {stop, IdStart}.

next_id(IdList, StartFrom) when is_integer(StartFrom), StartFrom >= 1 ->
  F = fun find_least_avail_id/2,
  {_, LeastAvailId} = lists:foldl(F, {continue, StartFrom}, IdList),
  LeastAvailId.


next_id_test() ->
  IdList = [1, 2, 3, 100, 101, 102, 110, 200],

  ?assertEqual(next_id(IdList, 1), 4),
  ?assertEqual(next_id(IdList, 100), 103),
  ?assertEqual(next_id(IdList, 120), 120),

  ?assertEqual(next_id([], 1), 1).


%%====================================================================
%% Internal functions
%%====================================================================
new_test(repo) ->
  new(?TEST_REPO, #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>, update_ts=>100});
new_test(model) ->
  new(?TEST_MODEL, #{id=>1, mcht_full_name=><<"full">>, mcht_short_name=><<"short">>, update_ts=>100}).


