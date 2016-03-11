%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
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

%% @doc emqttd plugin template
-module(emqttd_plugin_template).

-include("../../../include/emqttd.hrl").

-export([load/1, unload/0]).

%% Hooks functions
-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/3, on_client_subscribe_after/3, on_client_unsubscribe/3]).

-export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/3, [Env]),
    emqttd:hook('client.subscribe.after', fun ?MODULE:on_client_subscribe_after/3, [Env]),
    emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3, [Env]),
    emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
    emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
    emqttd:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]).

on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]),
    {ok, Client}.

on_client_disconnected(Reason, ClientId, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]),
    ok.

%% should retain TopicTable
on_client_subscribe(ClientId, TopicTable, _Env) ->
    io:format("client ~s will subscribe ~p~n", [ClientId, TopicTable]),
    {ok, TopicTable}.
   
on_client_subscribe_after(ClientId, TopicTable, _Env) ->
    io:format("client ~s subscribed ~p~n", [ClientId, TopicTable]),
    {ok, TopicTable}.
    
on_client_unsubscribe(ClientId, Topics, _Env) ->
    io:format("client ~s unsubscribe ~p~n", [ClientId, Topics]),
    {ok, Topics}.

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("publish ~s~n", [emqttd_message:format(Message)]),
    {ok, Message}.

on_message_delivered(ClientId, Message, _Env) ->
    io:format("delivered to client ~s: ~s~n", [ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

on_message_acked(ClientId, Message, _Env) ->
    io:format("client ~s acked: ~s~n", [ClientId, emqttd_message:format(Message)]),
    {ok, Message}.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
    emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/3),
    emqttd:unhook('client.subscribe.after', fun ?MODULE:on_client_subscribe_after/3),
    emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3),
    emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/3),
    emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/3).

