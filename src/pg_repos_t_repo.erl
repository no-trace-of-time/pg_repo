%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2016 8:17 PM
%%%-------------------------------------------------------------------
-module(pg_repos_t_repo).
-compile({parse_trans, exprecs}).
-behaviour(bh_exprecs).
-behavior(bh_repo).
-author("simon").

%% API
%% callbacks
-export([
  %% table define related
  table_config/0
]).

-export([
  field_pr_formatter/1
  , get/2
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL, mchants).

-type ts() :: erlang:timestamp().

-type id() :: non_neg_integer().
-type name() :: binary().
-type status() :: normal | forizon | closed.
-type payment_method() :: [gw_netbank | gw_wap | gw_app].


-export_type([id/0, name/0, status/0]).

-record(?TBL, {
  id = 0 :: id()
  , mcht_full_name = <<"">> :: name()
  , mcht_short_name = <<"">> :: name()
  , status = normal :: status()
  , payment_method = [gw_netbank] :: payment_method()
  , up_mcht_id = <<"">> :: binary()
  , quota = [{txn, -1}, {daily, -1}, {monthly, -1}] :: list()
  , up_term_no = <<"12345678">> :: binary()
  , update_ts = erlang:timestamp() :: ts()
  , field
}).
-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
    table_indexes => [mcht_full_name]
    , data_init => []
    , pk_is_sequence => true
    , pk_key_name => id
    , pk_type => integer

    , unique_index_name => mcht_full_name
    , query_option =>
  #{
    mcht_full_name => within
    , mcht_short_name => within
    , payment_method => member
  }

  }.

get(Repo, aa) when is_record(Repo, ?TBL) ->
  {get(Repo, id), get(Repo, mcht_full_name)};
get(Repo, Key) when is_record(Repo, ?TBL), is_atom(Key) ->
  pg_repos:get_in(?MODULE, Repo, Key).


%%-----------------------------------------------------------
field_pr_formatter(Field)
  when (Field =:= mcht_full_name)
  or (Field =:= mcht_short_name)
  ->
  "~p=~ts,";
field_pr_formatter(_) ->
  "~p=~p,".
