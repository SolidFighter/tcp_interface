%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2015 7:42 PM
%%%-------------------------------------------------------------------
-module(ti_app).
-author("myang").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

-define(DEFAULT_PORT, 1188).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  Port = case application:get_env(tcp_interface, port) of
           {ok, P} -> P;
           undefined -> ?DEFAULT_PORT
         end,
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  case ti_sup:start_link(LSock) of
    {ok, Pid} ->
      ti_sup:start_child(),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
