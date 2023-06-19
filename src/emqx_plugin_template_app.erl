{{=@@ @@=}}
-module(@@name@@_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = @@name@@_sup:start_link(),
    @@name@@:load(application:get_all_env()),

    emqx_ctl:register_command(@@name@@, {@@name@@_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(@@name@@),
    @@name@@:unload().
