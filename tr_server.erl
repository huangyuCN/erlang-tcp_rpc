%%%-------------------------------------------------------------------
%%% @author HuangYu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 三月 2016 下午4:24
%%%-------------------------------------------------------------------
-module(tr_server).
-include_lib("eunit/include/eunit.hrl").
-author("HuangYu").

-behaviour(gen_server).

%% API
-export([start_link/1,start_link/0, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%将SERVER设置为模块名
-define(SERVER, ?MODULE).
%%定义默认端口
-define(DEFAULT_PORT, 1055).
%%用于保存进程运行时状态
-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================
start_test()->
  {ok,_}=tr_server:start_link(1055).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%%启动服务器，启动并链接一个gen_server容器进程
%%启动tr_server并监听Port端口
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
%%启动tr_server并使用默认端口
start_link() ->
  start_link(?DEFAULT_PORT).

%%tr_server这个进程可以接受下列简单消息：get_count,stop
%%gen_server:call/2,向gen_server进程发送同步消息并等待应答
%%查询服务器已经处理的请求数
get_count() ->
  gen_server:call(?SERVER, get_count). %调用方会等待应答

%%请求终止服务器
%%gen_server:cast,向gen_server发送异步消息
stop() ->
  gen_server:cast(?SERVER, stop). %无需等待应答

do_rpc(Socket, RawData) ->
  try
    {M, F, A} = split_out_mfa(RawData),
    Result = apply(M, F, A),
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
    _Class:Err ->
      gen_tcp:send(Socket, io_lib:fwrite("~p~n",[Err]))
  end.

split_out_mfa(RawData) ->
  MFA = re:replace(RawData,
    "\r\n$", "", [{return, list}]),
  {match, [M, F, A]} =
    re:run(MFA,
      "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
      [{capture, [1, 2, 3], list}, ungreedy]),
  {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
  {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "].", 1),
  {ok, Args} = erl_parse:parse_term(Toks),
  Args.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Port]) ->
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  {ok, #state{port = Port, lsock = LSock}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, RawData}, State) ->
  do_rpc(Socket, RawData),
  RequestCount = State#state.request_count,
  {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
