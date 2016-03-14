%%%-------------------------------------------------------------------
%%% @author HuangYu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 三月 2016 上午11:47
%%%-------------------------------------------------------------------
{application, tcp_rpc, [
  {description, "RPC Server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules,[tr_app,tr_sup,tr_server]},
  {registered, [tr_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {tr_app, []}},
  {env, []}
]}.