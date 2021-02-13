%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_plugin_template).

-include_lib("emqx/include/emqx.hrl").

-export([ load/0
  , unload/0
]).

%% Client Lifecircle Hooks
-export([ on_client_connect/3
  , on_client_connack/4
  , on_client_connected/3
  , on_client_disconnected/4
  , on_client_authenticate/3
  , on_client_check_acl/5
  , on_client_subscribe/4
  , on_client_unsubscribe/4
]).

%% Session Lifecircle Hooks
-export([ on_session_created/3
  , on_message_fetch_for_queue/4
  , on_message_fetch_for_pubsub/4
  , on_session_unsubscribed/4
  , on_session_resumed/3
  , on_session_discarded/3
  , on_session_takeovered/3
  , on_session_terminated/4
]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
  , on_message_delivered/3
  , on_message_acked_for_queue_interface/3
  , on_message_acked_for_pubsub_interface/3
  , on_message_dropped/4
]).

%% Called when the plugin application start
%%load(Env) ->
%%  emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
%%  emqx:hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
%%  emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
%%  emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
%%  emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
%%  emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
%%  emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
%%  emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
%%  emqx:hook('session.created',     {?MODULE, on_session_created, [Env]}),
%%  emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
%%  emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
%%  emqx:hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
%%  emqx:hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
%%  emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
%%  emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
%%  emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
%%  emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
%%  emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
%%  emqx:hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

load() ->
  HookList =
    parse_hook(application:get_env(emqx_plugin_template,
      hooks, [])),
  lists:foreach(fun ({Hook, Action, Filter}) ->
    case proplists:get_value(<<"function">>, Action) of
      undefined ->
        {ok};
      Fun ->
        load_(Hook, b2a(Fun), {Filter, undefined})
    end
                end,
    HookList),
  io:format("~s is loaded.~n", [emqx_plugin_template]),
  ok.

%%--------------------------------------------------------------------
%% Redis Helper functions
%%--------------------------------------------------------------------

on_client_connected_redis(ConnInfo = #{clientid := ClientId}) ->
  io:format("on_client_connect_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_client_connected(#{clientid => ClientId}, ConnInfo, {undefined,RedisPool}),
  io:format("on_client_connect_redis executed\n").

on_client_disconnected_redis(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo) ->
  io:format("on_client_disconnect_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_client_disconnected(#{clientid => ClientId}, ReasonCode, ConnInfo, {undefined,RedisPool}),
  io:format("on_client_disconnect_redis executed\n").

on_message_fetch_for_queue_redis(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  io:format("on_message_fetch_for_queue_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_message_fetch_for_queue(#{clientid => ClientId}, Topic, Opts, {Filter,RedisPool}),
  io:format("on_message_fetch_for_queue_redis executed\n"),
  {ok}.

on_message_fetch_for_pubsub_redis(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  io:format("on_message_fetch_for_pubsub_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_message_fetch_for_pubsub(#{clientid => ClientId}, Topic, Opts, {Filter,RedisPool}),
  io:format("on_message_fetch_for_pubsub_redis executed\n"),
  {ok}.

on_message_acked_for_queue_redis(#{clientid := ClientId}, #message{topic = Topic, id = MsgId}, {Filter}) ->
  io:format("on_message_acked_for_queue_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_message_acked_for_queue(#{clientid => ClientId}, #message{topic = Topic, id = MsgId}, {Filter,RedisPool}),
  io:format("on_message_acked_for_queue_redis called\n").

on_message_acked_for_pubsub_redis(#{clientid := ClientId}, #message{topic = Topic, id = MsgId},{Filter}) ->
  io:format("on_message_acked_for_pubsub_redis called\n"),
  RedisPool = {single,emqx_backend_redis_pool1},
  emqx_backend_redis:on_message_acked_for_pubsub(#{clientid => ClientId}, #message{topic = Topic, id = MsgId}, {Filter,RedisPool}),
  io:format("on_message_acked_for_pubsub_redis called\n").

%%--------------------------------------------------------------------
%% Mysql Helper functions
%%--------------------------------------------------------------------

on_client_connected_mysql(ConnInfo = #{clientid := ClientId}) ->
  io:format("on_client_connect_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  emqx_backend_mysql:on_client_connected(#{clientid => ClientId}, ConnInfo, {undefined,MysqlPool}),
  io:format("on_client_connect_mysql executed\n").

on_client_disconnected_mysql(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo) ->
  io:format("on_client_disconnect_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  emqx_backend_mysql:on_client_disconnected(#{clientid => ClientId}, ReasonCode, ConnInfo, {undefined,MysqlPool}),
  io:format("on_client_disconnect_mysql executed\n").

on_message_fetch_for_queue_mysql(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  io:format("on_message_fetch_for_queue_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  OfflineOpts = [],
  emqx_backend_mysql:on_message_fetch(#{clientid => ClientId}, Topic, Opts, {Filter, MysqlPool, OfflineOpts}),
  io:format("on_message_fetch_for_queue_mysql executed\n").

on_message_fetch_for_pubsub_mysql(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  io:format("on_message_fetch_for_pubsub_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  OfflineOpts = [],
  emqx_backend_mysql:on_message_fetch(#{clientid => ClientId}, Topic, Opts, {Filter, MysqlPool, OfflineOpts}),
  io:format("on_message_fetch_for_pubsub_mysql executed\n").

on_message_acked_for_queue_mysql(ClientId, Topic, MsgId) ->
  io:format("on_message_acked_for_queue_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  emqx_metrics:inc('backend.mysql.on_message_acked'),
  emqx_backend_mysql_cli:custom_message_acked(MysqlPool, [{clientid, ClientId}, {topic, Topic}, {msgid, MsgId}]),
  io:format("on_message_acked_for_queue_mysql called\n").

on_message_acked_for_pubsub_mysql(ClientId, Topic, MsgId) ->
  io:format("on_message_acked_for_pubsub_mysql called\n"),
  MysqlPool = emqx_backend_mysql_pool1,
  emqx_metrics:inc('backend.mysql.on_message_acked'),
  emqx_backend_mysql_cli:custom_message_acked(MysqlPool, [{clientid, ClientId}, {topic, Topic}, {msgid, MsgId}]),
  io:format("on_message_acked_for_pubsub_mysql called\n").
%%--------------------------------------------------------------------
%% Client Lifecircle Hooks
%%--------------------------------------------------------------------

on_client_connect(ConnInfo = #{clientid := ClientId}, Props, _Env) ->
  io:format("Plugin template :: Client(~s) connect, ConnInfo: ~p, Props: ~p~n",
    [ClientId, ConnInfo, Props]),
  {ok, Props}.

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
  io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
    [ClientId, ConnInfo, Rc, Props]),
  {ok, Props}.

%%on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
%%  io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
%%    [ClientId, ClientInfo, ConnInfo]).

on_client_connected(#{clientid := ClientId}, _ConnInfo, {Filter}) ->
  io:format("on_client_connected plugin_template called , ClientId: ~p, ConnInfo: ~p, Filter: ~p~n",
    [ClientId, _ConnInfo, Filter]),
  with_filter(fun () ->
      on_client_connected_redis(_ConnInfo),
      on_client_connected_mysql(_ConnInfo)
              end,
    undefined, Filter).

%%on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
%%  io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
%%    [ClientId, ReasonCode, ClientInfo, ConnInfo]),
%%  on_client_disconnect_redis(ClientInfo, ReasonCode, ConnInfo, _Env),
%%  on_client_disconnect_mysql(ClientInfo, ReasonCode, ConnInfo, _Env),
%%  {ok}.

on_client_disconnected(ClientInfo = #{clientid := ClientId}, _Reason, _ConnInfo, {Filter}) ->
  io:format("on_client_disconnected plugin_template called , ClientId: ~p, Reason: ~p, ConnInfo: ~p, Filter: ~p~n",
    [ClientId, _Reason, _ConnInfo, Filter]),
  with_filter(fun () ->
    on_client_disconnected_redis(ClientInfo, _Reason, _ConnInfo),
    on_client_disconnected_mysql(ClientInfo, _Reason, _ConnInfo)
              end,
    undefined, Filter).

on_client_authenticate(_ClientInfo = #{clientid := ClientId}, Result, _Env) ->
  io:format("Client(~s) authenticate, Result:~n~p~n", [ClientId, Result]),
  {ok, Result}.

on_client_check_acl(_ClientInfo = #{clientid := ClientId}, Topic, PubSub, Result, _Env) ->
  io:format("Client(~s) check_acl, PubSub:~p, Topic:~p, Result:~p~n",
    [ClientId, PubSub, Topic, Result]),
  {ok, Result}.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
  {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
  {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
  io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

%%on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
%%  io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]),
%%  on_session_subscribed_redis(ClientId, Topic, SubOpts, _Env).

on_message_fetch_for_queue(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  with_filter(fun () ->
    on_message_fetch_for_queue_redis(ClientId, Topic, Opts, {Filter}),
    on_message_fetch_for_queue_mysql(ClientId, Topic, Opts, {Filter})
              end,
    Topic, Filter).

on_message_fetch_for_pubsub(#{clientid := ClientId}, Topic, Opts, {Filter}) ->
  with_filter(fun () ->
    on_message_fetch_for_pubsub_redis(ClientId, Topic, Opts, {Filter}),
    on_message_fetch_for_pubsub_mysql(ClientId, Topic, Opts, {Filter})
              end,
    Topic, Filter).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
  io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
  io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
  io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
  io:format("Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
  io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
    [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
%%on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
%%  {ok, Message};
%%
%%on_message_publish(Message, _Env) ->
%%  io:format("Publish ~s~n", [emqx_message:format(Message)]),
%%  {ok, Message}.

on_message_publish(Msg = #message{flags = #{retain := true}, payload = <<>>}, _Rule) ->
  {ok, Msg};
on_message_publish(Msg0 = #message{topic = Topic}, {Filter}) ->
  with_filter(fun () ->
    {ok, Msg0}
              end,
    Msg0, Topic, Filter).

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
  ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
  io:format("Message dropped by node ~s due to ~s: ~s~n",
    [Node, Reason, emqx_message:format(Message)]).

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
  io:format("Message delivered to client(~s): ~s~n",
    [ClientId, emqx_message:format(Message)]),
  {ok, Message}.

%%on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
%%  io:format("Message acked by client(~s): ~s~n",
%%    [ClientId, emqx_message:format(Message)]).

on_message_acked_for_queue_interface(#{clientid := ClientId}, #message{topic = Topic, id = MsgId}, {Filter}) ->
  io:format("emqx_plugin_template on_message_acked_for_queue_interface started"),
  with_filter(fun () ->
    on_message_acked_for_queue_redis(#{clientid => ClientId}, #message{topic = Topic, id = MsgId}, {Filter}),
    on_message_acked_for_queue_mysql(ClientId, Topic, MsgId)
              end,
    Topic, Filter).

on_message_acked_for_pubsub_interface(#{clientid := ClientId}, #message{topic = Topic, id = MsgId},{Filter}) ->
  io:format("emqx_plugin_template on_message_acked_for_pubsub_interface started"),
  with_filter(fun () ->
    on_message_acked_for_pubsub_redis(#{clientid => ClientId}, #message{topic = Topic, id = MsgId},{Filter}),
    on_message_acked_for_pubsub_mysql(ClientId, Topic, MsgId)
              end,
    Topic, Filter).

%% Called when the plugin application stop
%%unload() ->
%%  emqx:unhook('client.connect',      {?MODULE, on_client_connect}),
%%  emqx:unhook('client.connack',      {?MODULE, on_client_connack}),
%%  emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
%%  emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
%%  emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
%%  emqx:unhook('client.check_acl',    {?MODULE, on_client_check_acl}),
%%  emqx:unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
%%  emqx:unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
%%  emqx:unhook('session.created',     {?MODULE, on_session_created}),
%%  emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
%%  emqx:unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
%%  emqx:unhook('session.resumed',     {?MODULE, on_session_resumed}),
%%  emqx:unhook('session.discarded',   {?MODULE, on_session_discarded}),
%%  emqx:unhook('session.takeovered',  {?MODULE, on_session_takeovered}),
%%  emqx:unhook('session.terminated',  {?MODULE, on_session_terminated}),
%%  emqx:unhook('message.publish',     {?MODULE, on_message_publish}),
%%  emqx:unhook('message.delivered',   {?MODULE, on_message_delivered}),
%%  emqx:unhook('message.acked',       {?MODULE, on_message_acked}),
%%  emqx:unhook('message.dropped',     {?MODULE, on_message_dropped}).


unload() ->
  HookList =
    parse_hook(application:get_env(emqx_plugin_template,
      hooks, [])),
  lists:foreach(fun ({Hook, Action, _Filter}) ->
    case proplists:get_value(<<"function">>, Action) of
      undefined -> {ok};
      Fun -> unload_(Hook, b2a(Fun))
    end
                end,
    HookList),
  io:format("~s is unloaded.~n", [emqx_plugin_template]),
  ok.

unload_(Hook, Fun) ->
  case Hook of
    'client.connected' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/3);
    'client.disconnected' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/4);
    'session.subscribed' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/4);
    'session.unsubscribed' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/4);
    'message.publish' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/2);
    'message.acked' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/3);
    'message.delivered' ->
      emqx:unhook(Hook, fun emqx_plugin_template:Fun/3)
  end.

parse_hook(Hooks) -> parse_hook(Hooks, []).

parse_hook([], Acc) -> Acc;
parse_hook([{Hook, Item} | Hooks], Acc) ->
  Params = emqx_json:decode(Item),
  Action = proplists:get_value(<<"action">>, Params),
  Filter = proplists:get_value(<<"topic">>, Params),
  parse_hook(Hooks,
    [{l2a(Hook), Action, Filter}
      | Acc]).

l2a(L) -> erlang:list_to_atom(L).

load_(Hook, Fun, {Filter, undefined}) ->
  load_(Hook, Fun, {Filter});
load_(Hook, Fun, Params) ->
  case Hook of
    'client.connected' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/3, [Params]);
    'client.disconnected' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/4, [Params]);
    'session.subscribed' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/4, [Params]);
    'session.unsubscribed' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/4, [Params]);
    'message.publish' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/2, [Params]);
    'message.acked' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/3, [Params]);
    'message.delivered' ->
      emqx:hook(Hook, fun emqx_plugin_template:Fun/3, [Params])
  end.

b2a(B) -> erlang:binary_to_atom(B, utf8).

with_filter(Fun, _, undefined) -> Fun(), ok;
with_filter(Fun, Topic, Filter) ->
  case emqx_topic:match(Topic, Filter) of
    true -> Fun(), ok;
    false -> ok
  end.

with_filter(Fun, _, _, undefined) -> Fun();
with_filter(Fun, Msg, Topic, Filter) ->
  case emqx_topic:match(Topic, Filter) of
    true -> Fun();
    false -> {ok, Msg}
  end.