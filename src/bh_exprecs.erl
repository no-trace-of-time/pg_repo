%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十月 2017 22:48
%%%-------------------------------------------------------------------
-module(bh_exprecs).
-author("simon").

%% API
-export([]).

%% callbacks
-callback field_pr_formatter(Field :: atom()) -> list().
-callback get(Repo :: tuple(), Key :: atom()) -> any().
-callback get(Repo :: tuple(), Key :: atom(), Default :: any()) -> any().
