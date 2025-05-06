-module(emqx_pt_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% CT Setup
%%--------------------------------------------------------------------

all() ->
    [
        {group, avsc},
        {group, no_avsc}
    ].

groups() ->
    [
        {avsc, [], [t_authentication_smoke, t_authorization_smoke]},
        {no_avsc, [], [t_authentication_smoke, t_authorization_smoke]}
    ].

init_per_suite(Config) ->
    ok = emqx_pt_test_helpers:start(),
    ok = emqx_pt_test_api_helpers:delete_all_plugins(),
    Config.

end_per_suite(Config) ->
    ok = emqx_pt_test_helpers:stop(),
    Config.

init_per_group(avsc, Config) ->
    PluginName = <<"my_emqx_plugin_avsc">>,
    PluginNameVsn = <<(PluginName)/binary, "-1.0.0">>,
    [{plugin_name_vsn, PluginNameVsn}, {plugin_name, PluginName} | Config];
init_per_group(no_avsc, Config) ->
    PluginName = <<"my_emqx_plugin">>,
    PluginNameVsn = <<(PluginName)/binary, "-1.0.0">>,
    [{plugin_name_vsn, PluginNameVsn}, {plugin_name, PluginName} | Config].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    PluginNameVsn = ?config(plugin_name_vsn, Config),
    %% install plugin
    ok = emqx_pt_test_helpers:allow_plugin_install(PluginNameVsn),
    Filename = filename:join([
        code:lib_dir(emqx_pt), "test", "assets", <<PluginNameVsn/binary, ".tar.gz">>
    ]),
    ct:pal("Uploading plugin ~s~n", [Filename]),
    ok = emqx_pt_test_api_helpers:upload_plugin(Filename),
    ok = emqx_pt_test_api_helpers:start_plugin(PluginNameVsn),
    Config.

end_per_testcase(_Case, Config) ->
    PluginNameVsn = ?config(plugin_name_vsn, Config),
    ok = emqx_pt_test_api_helpers:configure_plugin(PluginNameVsn, default_config()),
    ok = emqx_pt_test_api_helpers:delete_all_plugins(),
    ok.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

t_authentication_smoke(Config) ->
    erlang:process_flag(trap_exit, true),
    PluginNameVsn = ?config(plugin_name_vsn, Config),
    PluginName = ?config(plugin_name, Config),
    %% Check authentication hook

    %% Connect with valid clientid
    ValidClientId = <<"valid_client_id">>,
    {ok, Pid0} = emqtt:start_link(connect_opts([{clientid, ValidClientId}])),
    {ok, _} = emqtt:connect(Pid0),
    ok = emqtt:disconnect(Pid0),

    %% Connect with invalid clientid, we should fail
    InvalidClientId = <<"invalid#clientid">>,
    {ok, Pid1} = emqtt:start_link(connect_opts([{clientid, InvalidClientId}])),
    {error, {malformed_username_or_password, undefined}} = emqtt:connect(Pid1),

    %% Verify that the config got by CLI is correct
    PluginConfigJson0 = emqx_pt_test_helpers:emqx_ctl([PluginName, "get-config"]),
    PluginConfig0 = jiffy:decode(PluginConfigJson0, [return_maps]),
    #{<<"client_regex">> := <<"^[A-Za-z0-9_]+$">>} = PluginConfig0,

    %% Update the config to allow the invalid clientid
    NewPluginConfig0 = config(#{client_regex => <<"^[A-Za-z0-9_#]+$">>}),
    emqx_pt_test_api_helpers:configure_plugin(PluginNameVsn, NewPluginConfig0),

    %% Connect with invalid clientid again, no we should not fail
    {ok, Pid2} = emqtt:start_link(connect_opts([{clientid, InvalidClientId}])),
    {ok, _} = emqtt:connect(Pid2),
    ok = emqtt:disconnect(Pid2),

    %% Verify that the config got by CLI is correct
    PluginConfigJson1 = emqx_pt_test_helpers:emqx_ctl([PluginName, "get-config"]),
    PluginConfig1 = jiffy:decode(PluginConfigJson1, [return_maps]),
    #{<<"client_regex">> := <<"^[A-Za-z0-9_#]+$">>} = PluginConfig1.

t_authorization_smoke(_Config) ->
    {ok, Pid} = emqtt:start_link(connect_opts([{clientid, <<"client_id">>}])),
    {ok, _} = emqtt:connect(Pid),

    %% Verify that hook effectively forbids subscribing to arbitrary topics
    %% Fails
    {ok, _, [128]} = emqtt:subscribe(Pid, <<"test/topic">>, 1),
    %% Succeeds
    {ok, _, [1]} = emqtt:subscribe(Pid, <<"room/client_id">>, 1),

    %% Cleanup
    ok = emqtt:disconnect(Pid).

receive_messages() ->
    receive
        {publish, #{payload := Payload}} ->
            [binary_to_integer(Payload) | receive_messages()]
    after 500 ->
        []
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

connect_opts(Opts) ->
    Opts ++ [{host, "127.0.0.1"}, {port, 1883}].

config(Overrides) ->
    maps:merge(default_config(), Overrides).

default_config() ->
    #{
        client_regex => <<"^[A-Za-z0-9_]+$">>,
        hostname => <<"localhost">>,
        port => 3306,
        connectionOptions => [
            #{
                optionName => <<"autoReconnect">>,
                optionType => <<"string">>,
                optionValue => <<"true">>
            }
        ],
        auth => #{
            username => <<"admin">>,
            password => #{
                string => <<"Public123">>
            }
        }
    }.
