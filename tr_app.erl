%%%-------------------------------------------------------------------
%%% @author HuangYu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 应用的行为模式实现模块（命名方式通常为，应用名称_app）
%%% @end
%%% Created : 14. 三月 2016 上午11:37
%%%-------------------------------------------------------------------
-module(tr_app).
-author("HuangYu").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

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

%%启动根监督者
start(_Type, _StartArgs) ->
  case tr_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error,Other}
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
