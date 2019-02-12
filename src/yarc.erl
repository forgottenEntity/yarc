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

-define(DEFAULT_TIMEOUT, 5000).

%% API

-export([get_item_json/2,
         put_item_json/3,
         get_item_map/2,
         put_item_map/3,
         delete_item/2
]).


%% ******************************************************************************
%% Erlang Application Functions...
%% ******************************************************************************


stop(_Application) ->
  ok.

start(_Application, _Type) ->
  Pid = supervisor:start_link({local, riak_pool_supervisor}, yarc_riak_connection_pool, []),
  YarcSchema = yarc_schema:new(),
  application:set_env(yarc, yarc_schema_cache, YarcSchema),
  io:format("~p~n", [Pid]),
  Pid.


%%====================================================================
%% API functions
%%====================================================================

get_item_json(Store, Key) ->
  case get_item_map(Store, Key) of
    Definition when is_map(Definition) ->
      jsx:encode(Definition);
    AnythingElse ->
      AnythingElse
  end.

put_item_json(Store, Key, Object) ->
  case get_schema(Store) of
    Definition when is_binary(Definition) ->
      FinalObjectMap = process_object(Object, Definition),
      put_object(Store, Key, jsx:encode(FinalObjectMap));
    AnythingElse ->
      AnythingElse
  end.


get_item_map(Store, Key) ->
  case get_schema(Store) of
    Definition when is_binary(Definition) ->
      case get_object(Store, Key) of
        Object when is_binary(Object) ->
          process_object(Object, Definition);
        Error ->
          Error
      end;
    AnythingElse ->
      AnythingElse
  end.

put_item_map(Store, Key, Map) ->
  case get_schema(Store) of
    Definition when is_binary(Definition) ->
      DefinitionMap = jsx:decode(Definition, [return_maps]),
      FinalObjectMap = process_object_map(Map, DefinitionMap),
      put_object(Store, Key, jsx:encode(FinalObjectMap));
    AnythingElse ->
      AnythingElse
  end.



delete_item(Store, Key) ->
  ok.




%%====================================================================
%% Internal functions
%%====================================================================

get_schema(Store) ->
  {ok, SchemaCache} = application:get_env(yarc, yarc_schema_cache),
  yarc_schema:get_definition(Store, SchemaCache).

get_object(Store, Key) ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  Object = case yarc_riak_connection:get(RiakConnection, Store, Key, ?DEFAULT_TIMEOUT) of
                 {ok, RiakObj} ->
                   riakc_obj:get_value(RiakObj);
                 Error ->
                   Error
               end,
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  Object.


put_object(Store, Key, Object) ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  RiakObj = riakc_obj:new(Store, Key, Object),
  yarc_riak_connection:put(RiakConnection, ?DEFAULT_TIMEOUT, RiakObj),
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  ok.

process_object(Object, Definition) ->
  ObjectMap = jsx:decode(Object, [return_maps]),
  DefinitionMap = jsx:decode(Definition, [return_maps]),
  process_object_map(ObjectMap, DefinitionMap).

process_object_map(ObjectMap, DefinitionMap) ->
  FilteredObjectMap = maps:with(maps:keys(DefinitionMap), ObjectMap),
  remove_deleted_fields(FilteredObjectMap, DefinitionMap).

remove_deleted_fields(ObjectMap, DefinitionMap) ->
  FilteredDefinition = yarc_schema_definition:filter_deleted_definition_entries(DefinitionMap),
  maps:with(maps:keys(FilteredDefinition), ObjectMap).

