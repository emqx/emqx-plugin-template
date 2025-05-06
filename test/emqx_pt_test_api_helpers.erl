-module(emqx_pt_test_api_helpers).

%% Plugin API
-export([
    upload_plugin/1,
    list_plugins/0,
    get_plugin/1,
    start_plugin/1,
    stop_plugin/1,
    delete_plugin/1,
    delete_all_plugins/0,
    configure_plugin/2
]).

%%--------------------------------------------------------------------
%% Plugin API
%%--------------------------------------------------------------------

upload_plugin(Filename) ->
    Parts = [
        {file, Filename, <<"plugin">>, []}
    ],
    case
        emqx_pt_test_helpers:api_post_raw(
            {plugins, install},
            [],
            {multipart, Parts}
        )
    of
        {ok, _} ->
            ok;
        {error, Error} ->
            error(Error)
    end.

list_plugins() ->
    case emqx_pt_test_helpers:api_get(plugins) of
        {ok, Plugins} when is_list(Plugins) ->
            Plugins;
        {error, Error} ->
            error(Error)
    end.

get_plugin(PluginId) ->
    case emqx_pt_test_helpers:api_get({plugins, PluginId}) of
        {ok, Plugin} ->
            Plugin;
        {error, Error} ->
            error(Error)
    end.

plugin_id(#{<<"name">> := Name, <<"rel_vsn">> := RelVsn}) ->
    <<Name/binary, "-", RelVsn/binary>>.

start_plugin(PluginId) ->
    case emqx_pt_test_helpers:api_put_raw({plugins, PluginId, start}, <<>>) of
        {ok, _} ->
            ok;
        {error, Error} ->
            error(Error)
    end.

stop_plugin(PluginId) ->
    case emqx_pt_test_helpers:api_put_raw({plugins, PluginId, stop}, <<>>) of
        {ok, _} ->
            ok;
        {error, Error} ->
            error(Error)
    end.

delete_plugin(PluginId) ->
    emqx_pt_test_helpers:api_delete({plugins, PluginId}).

delete_all_plugins() ->
    Plugins = list_plugins(),
    lists:foreach(
        fun(Plugin) ->
            ok = stop_plugin(plugin_id(Plugin)),
            ok = delete_plugin(plugin_id(Plugin))
        end,
        Plugins
    ).

configure_plugin(PluginId, Config) ->
    case emqx_pt_test_helpers:api_put({plugins, PluginId, config}, Config) of
        ok ->
            ok;
        {error, Error} ->
            error(Error)
    end.
