%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 十月 2017 10:08
%%%-------------------------------------------------------------------
-module(pg_repo_SUITE).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([]).

-define(TableName, mchants).
-define(TEST_REPO, pg_repo_t_repo).
-define(TEST_REPO_TABLE, ?TableName).
-define(TEST_MODEL, pg_repo_t_model).

setup() ->
  lager:start(),
  setup(mnesia),

  ok.


%%--------------------------------------------------------------------
cleanup(_) ->
  mnesia:delete_table(?TableName),
  ok.
%%--------------------------------------------------------------------
setup(mnesia) ->
  Dir = "/tmp/mnesia_test",
  os:cmd("mkdir " ++ Dir),
  application:set_env(mnesia, dir, Dir),
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),

  ok = application:start(mnesia),
  ok.
%%---------------------------------------------------------------------
-spec db_init(Cfgs :: proplists:proplist()) -> ok.

db_init(Cfgs) when is_list(Cfgs) ->
  F =
    fun(M, ValueList) ->
      pg_repo:drop(M),
      pg_repo:init(M),
      [db_init_one_row(M, VL) || VL <- ValueList]
    end,

  [F(M, DataList) || {M, DataList} <- Cfgs],

  ok.

db_init_one_row(M, VL) ->
  Repo = pg_model:new(M, VL),
  pg_repo:save(Repo).

%%---------------------------------------------------------------------
my_test_() ->
  {
    setup,
    fun setup/0,
    fun cleanup/1,
    {
      inorder,
      [
        fun init_test_1/0
        , fun index_test_1/0
        , fun create_test_1/0
        , fun read_test_1/0
        , fun save_test_1/0
        , fun delete_test_1/0

        , fun clean_up_record_list_test_1/0
      ]
    }
  }.

%%---------------------------------------------------------------------
repo_init() ->
  pg_repo:drop(?TEST_REPO),
  pg_repo:init(?TEST_REPO),
  mnesia:wait_for_tables([?TableName], 5000),
  ok.


init_test_1() ->
  repo_init(),

  ?assertEqual(mchants, mnesia:table_info(?TableName, record_name)),
  ?assertEqual([3], mnesia:table_info(?TEST_REPO_TABLE, index)),

  ok.

index_test_1() ->
  repo_init(),

  pg_repo:drop_index(?TEST_REPO),
  ?assertEqual([], mnesia:table_info(?TEST_REPO_TABLE, index)),

  pg_repo:create_index(?TEST_REPO),
  ?assertEqual([3], mnesia:table_info(?TEST_REPO_TABLE, index)),

  pg_repo:recreate_index(?TEST_REPO),
  ?assertEqual([3], mnesia:table_info(?TEST_REPO_TABLE, index)),
  ok.


create_test_1() ->
  repo_init(),

  PK = 101,
  FullName = <<"ttt">>,
  pg_repo:create_pk(?TEST_REPO, PK, [{mcht_full_name, FullName}]),
  [R] = mnesia:dirty_read(?TableName, PK),
  ?assertEqual(PK, pg_model:get(?TEST_REPO, R, id)),
  ?assertEqual(FullName, pg_model:get(?TEST_REPO, R, mcht_full_name)),

  FullName2 = <<"sss">>,
  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName2}, []),
  [R2] = mnesia:dirty_read(?TableName, 1),
  ?assertEqual(FullName2, pg_model:get(?TEST_REPO, R2, mcht_full_name)),

  FullName3 = <<"aaa">>,
  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName3}, #{mcht_short_name=>FullName3}),
  [R3] = mnesia:dirty_read(?TableName, 2),
  ?assertEqual(FullName3, pg_model:get(?TEST_REPO, R3, mcht_full_name)),
  ?assertEqual(FullName3, pg_model:get(?TEST_REPO, R3, mcht_short_name)),
  ok.

read_test_1() ->
  repo_init(),

  FullName2 = <<"sss">>,
  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName2}, []),
  [R2] = pg_repo:read(?TEST_REPO, 1),
  ?assertEqual(FullName2, pg_model:get(?TEST_REPO, R2, mcht_full_name)),

  ?assertEqual([R2], pg_repo:read_index(?TEST_REPO, mcht_full_name, FullName2)),
  ?assertEqual([R2], pg_repo:read_index(?TEST_REPO, {mcht_full_name, FullName2})),

  %% fetch_by
  ?assertEqual([1, <<"sss">>], pg_repo:fetch_by(?TEST_REPO, 1, [id, mcht_full_name])),

  ok.

save_test_1() ->
  repo_init(),

  FullName2 = <<"aaa">>,
  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName2}, []),
  [R2] = pg_repo:read(?TEST_REPO, 1),

  NewFullName2 = <<"bbb">>,
  R3 = pg_model:set(?TEST_REPO, R2, mcht_full_name, NewFullName2),
  ?assertEqual(ok, pg_repo:save(R3)),
  [R31] = pg_repo:read(?TEST_REPO, 1),
  ?assertEqual(NewFullName2, pg_model:get(?TEST_REPO, R31, mcht_full_name)),

  NewFullName3 = <<"ccc">>,
  R4 = pg_model:set(?TEST_REPO, R2, mcht_full_name, NewFullName3),
  ?assertEqual(ok, pg_repo:save(?TEST_REPO, R4)),
  [R41] = pg_repo:read(?TEST_REPO, 1),
  ?assertEqual(NewFullName3, pg_model:get(?TEST_REPO, R41, mcht_full_name)),

  NewFullName4 = <<"ddd">>,
  R5 = pg_model:set(?TEST_REPO, R2, mcht_full_name, NewFullName4),
  ?assertEqual(ok, pg_repo:save(?TEST_REPO, pg_model:to(?TEST_REPO, R5, map))),
  [R51] = pg_repo:read(?TEST_REPO, 1),
  ?assertEqual(NewFullName4, pg_model:get(?TEST_REPO, R51, mcht_full_name)),

  NewFullName5 = <<"eee">>,
  R6 = pg_model:set(?TEST_REPO, R2, mcht_full_name, NewFullName5),
  ?assertEqual(ok, pg_repo:save(?TEST_REPO, pg_model:to(?TEST_REPO, R6, map), [sync])),
  [R61] = pg_repo:read(?TEST_REPO, 1),
  ?assertEqual(NewFullName5, pg_model:get(?TEST_REPO, R61, mcht_full_name)),

  ok.

delete_test_1() ->
  repo_init(),

  FullName2 = <<"aaa">>,
  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName2}, []),
  [R2] = pg_repo:read(?TEST_REPO, 1),

  pg_repo:delete(R2),
  ?assertEqual([], pg_repo:read(?TEST_REPO, 1)),

  pg_repo:create(?TEST_REPO, {mcht_full_name, FullName2}, []),
  ?assertEqual(ok, pg_repo:delete(?TEST_REPO, 1)),
  ?assertEqual([], pg_repo:read_index(?TEST_REPO, mcht_full_name, FullName2)),

  ok.
clean_up_record_list_test_1() ->
  Rec = [{id, 1}, {payment_method, [<<"gw_netbank">>]}, {status, <<"normal">>}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, <<"2016-12-25T21:25:04.012503 +08:00">>}, {date, <<"2016/11/12">>},
    {mcht_full_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183, 229, 133, 172, 229, 143, 184>>},
    {mcht_short_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183>>}, {mcht_full_name, <<>>}],
  RecExpected = [{id, 1}, {payment_method, [gw_netbank]}, {status, normal}, {up_mcht_id, <<"898350273922385">>},
    {update_ts, {1482, 672304, 12503}},
    {mcht_full_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183, 229, 133, 172, 229, 143, 184>>},
    {mcht_short_name, <<230, 152, 147, 229, 174, 182, 229, 129, 165, 229, 186, 183>>}, {mcht_full_name, <<>>}],

  ?assertEqual(RecExpected, lists:reverse(pg_repo:clean_up_record_list(?TEST_REPO, Rec))).
