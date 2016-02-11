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

-export([on_message_publish/2, on_message_acked/3]).

%% Called when the plugin application start
load(Env) ->

    emqttd_broker:hook('client.connected', {?MODULE, on_client_connected},
                       {?MODULE, on_client_connected, [Env]}),

    emqttd_broker:hook('client.disconnected', {?MODULE, on_client_disconnected},
                       {?MODULE, on_client_disconnected, [Env]}),

    emqttd_broker:hook('client.subscribe', {?MODULE, on_client_subscribe},
                       {?MODULE, on_client_subscribe, [Env]}),

    emqttd_broker:hook('client.subscribe.after', {?MODULE, on_client_subscribe_after},
                       {?MODULE, on_client_subscribe_after, [Env]}),

    emqttd_broker:hook('client.unsubscribe', {?MODULE, on_client_unsubscribe},
                       {?MODULE, on_client_unsubscribe, [Env]}),

    emqttd_broker:hook('message.publish', {?MODULE, on_message_publish},
                       {?MODULE, on_message_publish, [Env]}),

    emqttd_broker:hook('message.acked', {?MODULE, on_message_acked},
                       {?MODULE, on_message_acked, [Env]}).

    

on_client_connected(ConnAck, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]).

on_client_disconnected(Reason, ClientId, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]).

%% should retain TopicTable
on_client_subscribe(ClientId, TopicTable, _Env) ->
    io:format("client ~s will subscribe ~p~n", [ClientId, TopicTable]),
    TopicTable.
   
on_client_subscribe_after(ClientId, TopicTable, _Env) ->
    io:format("client ~s subscribed ~p~n", [ClientId, TopicTable]).
    
on_client_unsubscribe(ClientId, Topics, _Env) ->
    io:format("client ~s unsubscribe ~p~n", [ClientId, Topics]),
    Topics.

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    Message;

on_message_publish(Message, _Env) ->
    io:format("publish ~s~n", [emqttd_message:format(Message)]),
    Message.

on_message_acked(ClientId, Message, _Env) ->
    io:format("client ~s acked ~s", [ClientId, emqttd_message:format(Message)]).

%% Called when the plugin application stop
unload() ->
    emqttd_broker:unhook('client.connected', {?MODULE, on_client_connected}),

    emqttd_broker:unhook('client.disconnected', {?MODULE, on_client_disconnected}),

    emqttd_broker:unhook('client.subscribe', {?MODULE, on_client_subscribe}),

    emqttd_broker:unhook('client.subscribe.after', {?MODULE, on_client_subscribe_after}),

    emqttd_broker:unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),

    emqttd_broker:unhook('message.publish', {?MODULE, on_message_publish}),

    emqttd_broker:unhook('message.acked', {?MODULE, on_message_acked}).

