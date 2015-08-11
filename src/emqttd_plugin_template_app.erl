-module(emqttd_plugin_template_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_plugin_template_sup:start_link(),
    Env = application:get_all_env(),
    emqttd_plugin_template:onload(Env),
    {ok, Sup}.

stop(_State) ->
    emqttd_plugin_template:onunload().
