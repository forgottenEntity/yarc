%%%-------------------------------------------------------------------
%%% @author forgottenEntity
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2019 20:16
%%%-------------------------------------------------------------------
-module(yarc).
-author("forgottenEntity").

%% API
-behaviour(application).

-export([stop/1,
         start/2
        ]).


stop(_Application) ->
  ok.

start(_Application, _Type) ->
  Pid = supervisor:start_link({local, riak_pool_supervisor}, arc_riak_connection_pool, []),
  io:format("~p~n", [Pid]),
  Pid.


