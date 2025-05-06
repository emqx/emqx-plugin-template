{{=@@ @@=}}
-module(@@name@@_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

-export([
    on_config_changed/2,
    on_health_check/1
]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = @@name@@_sup:start_link(),
    @@name@@:hook(),
    emqx_ctl:register_command(@@name@@, {@@name@@_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(@@name@@),
    @@name@@:unhook().

on_config_changed(OldConfig, NewConfig) ->
    @@name@@:on_config_changed(OldConfig, NewConfig).

on_health_check(Options) ->
    @@name@@:on_health_check(Options).
