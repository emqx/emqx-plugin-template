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
    {ok, Sup}.

stop(_State) ->
    @@name@@:unload().
