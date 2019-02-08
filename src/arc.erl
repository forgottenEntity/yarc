%%%-------------------------------------------------------------------
%%% @author chris
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2019 20:16
%%%-------------------------------------------------------------------
-module(arc).
-author("chris").

%% API
-behaviour(application).

-export([stop/1,
         start/2
        ]).


stop(_Application) ->
  ok.

start(_Application, _Type) ->
  ok.


