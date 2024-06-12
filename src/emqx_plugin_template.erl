{{=@@ @@=}}
-module(@@name@@).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").


%% for logging
-include_lib("emqx/include/logger.hrl").

-export([ load/1
        , unload/0
        ]).

%% Client Lifecycle Hooks
-export([ on_client_connect/3
        , on_client_connack/4
        , on_client_connected/3
        , on_client_disconnected/4
        , on_client_authenticate/3
        , on_client_authorize/5
        , on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

%% Session Lifecycle Hooks
-export([ on_session_created/3
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_resumed/3
        , on_session_discarded/3
        , on_session_takenover/3
        , on_session_terminated/4
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_puback/5
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).

%% Called when the plugin application start
load(Env) ->
    hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
    hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    hook('client.authorize',    {?MODULE, on_client_authorize, [Env]}),
    hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    hook('session.created',     {?MODULE, on_session_created, [Env]}),
    hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    hook('session.takenover',   {?MODULE, on_session_takenover, [Env]}),
    hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    hook('message.puback',      {?MODULE, on_message_puback, [Env]}),
    hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, _Env) ->
    %% this is to demo the usage of EMQX's structured-logging macro
    %% * Recommended to always have a `msg` field,
    %% * Use underscore instead of space to help log indexers,
    %% * Try to use static fields
    ?SLOG(debug, #{msg => "demo_log_msg_on_client_connect",
                   conninfo => ConnInfo,
                   props => Props}),
    %% If you want to refuse this connection, you should return with:
    %% {stop, {error, ReasonCode}}
    %% the ReasonCode can be found in the emqx_reason_codes.erl
    {ok, Props}.

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
    io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
              [ClientId, ConnInfo, Rc, Props]),
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
    io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]).

%% @doc
%% - Return `{stop, ok}' if this client is to be allowed to login.
%% - Return `{stop, {error, not_authorized}}' if this client is not allowed to login.
%% - Return `ignore' if this client is to be authenticated by other plugins
%% or EMQX's built-in authenticators.
on_client_authenticate(ClientInfo = #{clientid := ClientId}, DefaultResult, Env) ->
    io:format("Client(~s) authenticate, ClientInfo:~n~p~n"
              "DefaultResult:~p~n"
              "Env:~p~n",
              [ClientId, ClientInfo, DefaultResult, Env]),
    DefaultResult.

%% @doc
%% - Return `{stop, #{result => Result}}' where `Result' is either `allow' or `deny'.
%% - Return `ignore' if this client is to be authorized by other plugins or
%% EMQX's built-in authorization sources.
on_client_authorize(ClientInfo = #{clientid := ClientId}, PubSub, Topic, DefaultResult, Env) ->
    io:format("Client(~s) authorize, ClientInfo:~n~p~n"
              "~p to topic (~p) DefaultResult:~p~n"
              "Env:~p~n",
              [ClientId, ClientInfo, PubSub, Topic, DefaultResult, Env]),
    %% `from' is for logging
    Result = #{result => allow, from => ?MODULE},
    {stop, Result}.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecycle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takenover(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takenover. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
              [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("Publish ~p~n", [emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_puback(_PacketId, #message{topic = _Topic} = Message, _PubRes, RC, _Env) ->
    NewRC = case RC of
                %% Demo: some service do not want to expose the error code (129) to client;
                %% so here it remap 129 to 128
                129 -> 128;
                _ -> RC
            end,
    io:format("Puback ~p RC: ~p~n",
              [emqx_message:to_map(Message), NewRC]),
    {ok, NewRC}.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
    io:format("Message dropped by node ~p due to ~p:~n~p~n",
              [Node, Reason, emqx_message:to_map(Message)]).

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message delivered to client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message acked by client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]).

%% Called when the plugin application stop
unload() ->
    unhook('client.connect',      {?MODULE, on_client_connect}),
    unhook('client.connack',      {?MODULE, on_client_connack}),
    unhook('client.connected',    {?MODULE, on_client_connected}),
    unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    unhook('client.authorize',    {?MODULE, on_client_authorize}),
    unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
    unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
    unhook('session.created',     {?MODULE, on_session_created}),
    unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
    unhook('session.resumed',     {?MODULE, on_session_resumed}),
    unhook('session.discarded',   {?MODULE, on_session_discarded}),
    unhook('session.takenover',   {?MODULE, on_session_takenover}),
    unhook('session.terminated',  {?MODULE, on_session_terminated}),
    unhook('message.publish',     {?MODULE, on_message_publish}),
    unhook('message.puback',      {?MODULE, on_message_puback}),
    unhook('message.delivered',   {?MODULE, on_message_delivered}),
    unhook('message.acked',       {?MODULE, on_message_acked}),
    unhook('message.dropped',     {?MODULE, on_message_dropped}).

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).
