%%%-------------------------------------------------------------------
%%% @author HuangYu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% 监督者行为模式模块的实现（命名方式为，应用名_sup)
%%% @end
%%% Created : 14. 三月 2016 上午11:01
%%%-------------------------------------------------------------------
-module(tr_sup).
-author("HuangYu").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% 启动监督者
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
%% 指明如何启动和管理子进程
%%  指明监督者的行为
%%  RestartStrategy是启动策略，它是一个三元组{How,Max,Within}
%%  one_for_one表示一旦子进程退出，监督者将针对给进程，且仅针对该进程进行重启
%%  返回监督规范
init([]) ->
  Server = {tr_server,{tr_server,start_link,[]},permanent,2000,worker,[tr_server]},
  Children = [Server],
  RestartStrategy = {one_for_one,0,1},
  {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
