{{=@@ @@=}}
-module(@@name@@).

-define(PLUGIN_NAME, "@@name@@").
-define(PLUGIN_VSN, "@@version@@").

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx_plugin_helper/include/emqx.hrl").

%% for hook priority constants
-include_lib("emqx_plugin_helper/include/emqx_hooks.hrl").

%% for logging
-include_lib("emqx_plugin_helper/include/logger.hrl").

-export([
    hook/0,
    unhook/0,
    start_link/0
]).

-export([
    on_config_changed/2,
    on_health_check/1,
    get_config/0
]).

%% Hook callbacks
-export([
    on_client_connect/3,
    on_client_authenticate/2,
    on_client_authorize/4,
    on_message_puback/4
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% @doc
%% Called when the plugin application start
hook() ->
    %% NOTE:
    %% We use highest hook priority ?HP_HIGHEST so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    %%
    %% We hook into several EMQX hooks for demonstration purposes.
    %% For the actual full list of hooks, their types and signatures see the EMQX code
    %% https://github.com/emqx/emqx/blob/master/apps/emqx/src/emqx_hookpoints.erl#L102
    %%
    %% We can pass additional arguments to the callback functions (see 'client.connect').
    emqx_hooks:add('client.connect', {?MODULE, on_client_connect, [some_arg]}, ?HP_HIGHEST),
    emqx_hooks:add('client.authenticate', {?MODULE, on_client_authenticate, []}, ?HP_HIGHEST),
    emqx_hooks:add('client.authorize', {?MODULE, on_client_authorize, []}, ?HP_HIGHEST),
    emqx_hooks:add('message.puback', {?MODULE, on_message_puback, []}, ?HP_HIGHEST).

%% @doc
%% Called when the plugin stops
unhook() ->
    emqx_hooks:del('client.connect', {?MODULE, on_client_connect}),
    emqx_hooks:del('client.authenticate', {?MODULE, on_client_authenticate}),
    emqx_hooks:del('client.authorize', {?MODULE, on_client_authorize}),
    emqx_hooks:del('message.puback', {?MODULE, on_message_puback}).

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------

%% NOTE
%% There are 2 types of hooks: fold hooks and run hooks.
%%
%% For run hooks, the registered callbacks are just sequentially called:
%% * Until all of them are called;
%% * Until one of them returns `stop`. In this case the subsequent callbacks
%% are not called.
%%
%% For fold hooks, the registered callbacks are sequentially called with the
%% accumulated value. The accumulated value is passed to the next callback as the
%% last argument coming before custom arguments.
%% The convention of the return values is the following:
%% * `{stop, Acc}` - stop calling the subsequent callbacks, return `Acc` as the result.
%% * `{ok, Acc}` - continue calling the subsequent callbacks, but use `Acc` as the new
%% accumulated value.
%% * `stop` - stop calling the subsequent callbacks, return current accumulator as the result.
%% * any other value - continue calling the subsequent callbacks without changing the accumulator.

%% The example of a run hook is the 'client.connected' hook. Callbacks for this hook
%% are called when a client is connected to the broker, no action required.
%%
%% The example of a fold hook is the 'client.authenticate' hook. Callbacks for this hook
%% are called when a client is authenticated, and may be used to handle authentication
%% and provide authentication result.

%% @doc
%% This is a run hook callback.
%% It does not do anything useful here, just demonstrates
%% * receiving additional arguments from the hook registration;
%% * using logging.
on_client_connect(ConnInfo, Props, some_arg) ->
    %% NOTE
    %% EMQX structured-logging macros should be used for logging.
    %%
    %% * Recommended to always have a `msg` field,
    %% * Use underscore instead of space to help log indexers,
    %% * Try to use static fields
    ?SLOG(debug, #{
        msg => "@@name@@_on_client_connect",
        conninfo => ConnInfo,
        props => Props
    }),
    {ok, Props}.

%% @doc
%% This a fold hook callback.
%%
%% - Return `{stop, ok}' if this client is to be allowed to login.
%% - Return `{stop, {error, not_authorized}}' if this client is not allowed to login.
%% - Return `ignore' if this client is to be authenticated by other plugins
%% or EMQX's built-in authenticators.
%%
%% Here we check if the clientid matches the regex in the config.
on_client_authenticate(#{clientid := ClientId} = _ClientInfo, DefaultResult) ->
    Config = get_config(),
    ClientIdRE = maps:get(<<"client_regex">>, Config, <<"">>),
    ?SLOG(debug, #{
        msg => "@@name@@_on_client_authenticate",
        clientid => ClientId,
        clientid_re => ClientIdRE
    }),
    case re:run(ClientId, ClientIdRE, [{capture, none}]) of
        match -> {ok, DefaultResult};
        nomatch -> {stop, {error, bad_username_or_password}}
    end.

%% @doc
%% This is a fold hook callback.
%%
%% - To permit/forbid actions, return `{stop, #{result => Result}}' where `Result' is either `allow' or `deny'.
%% - Return `ignore' if this client is to be authorized by other plugins or
%% EMQX's built-in authorization sources.
%%
%% Here we demonstrate the following rule:
%% Clients can only subscribe to topics formatted as /room/{clientid}, but can send messages to any topics.
on_client_authorize(
    _ClientInfo = #{clientid := ClientId}, #{action_type := subscribe} = Action, Topic, _Result
) ->
    ?SLOG(debug, #{
        msg => "@@name@@_on_client_authorize_subscribe",
        clientid => ClientId,
        topic => Topic,
        action => Action
    }),
    case emqx_topic:match(Topic, <<"room/", ClientId/binary>>) of
        true -> ignore;
        false -> {stop, #{result => deny, from => ?MODULE}}
    end;
on_client_authorize(_ClientInfo = #{clientid := ClientId}, Action, Topic, _Result) ->
    ?SLOG(debug, #{
        msg => "@@name@@_on_client_authorize",
        clientid => ClientId,
        topic => Topic,
        action => Action
    }),
    ignore.

%% @doc
%% Demo callback working with messages. This is a fold hook callback.
on_message_puback(_PacketId, #message{} = Message, PubRes, RC) ->
    NewRC =
        case RC of
            %% Demo: some service do not want to expose the error code (129) to client;
            %% so here it remap 129 to 128
            129 -> 128;
            _ -> RC
        end,
    ?SLOG(debug, #{
        msg => "@@name@@_on_message_puback",
        message => emqx_message:to_map(Message),
        pubres => PubRes,
        rc => NewRC
    }),
    {ok, NewRC}.

%%--------------------------------------------------------------------
%% Plugin callbacks
%%--------------------------------------------------------------------

%% @doc
%% - Return `{error, Error}' if the health check fails.
%% - Return `ok' if the health check passes.
%%
%% NOTE
%% For demonstration, we consider any port number other than 3306 unavailable.
on_health_check(_Options) ->
    case get_config() of
        #{<<"port">> := 3306} ->
            ok;
        #{<<"port">> := Port} ->
            {error, iolist_to_binary(io_lib:format("Port unavailable: ~p", [Port]))};
        _ ->
            {error, <<"Invalid config, no port">>}
    end.

%% @doc
%% - Return `{error, Error}' if the new config is invalid.
%% - Return `ok' if the config is valid and can be accepted.
%%
%% NOTE
%% We validate only the client_regex field here. Other config fields are present
%% only for the demonstration purposes.
%%
%% NOTE
%% Take the following considerations into account when writing the validating callback:
%% * You should not make validations depending on the environment, e.g.
%%   check network connection, file system, etc.
%% * The callback may be called even if the application is not running.
%%   Here we use `gen_server:cast/2` to react on changes. The cast will be silently
%%   ignored if the plugin is not running.
on_config_changed(_OldConfig, #{<<"client_regex">> := ClientRegex} = NewConfig) ->
    case re:compile(ClientRegex) of
        {ok, _} ->
            ok = gen_server:cast(?MODULE, {on_changed, NewConfig});
        {error, Error} ->
            {error, iolist_to_binary(io_lib:format("~p", [Error]))}
    end;
on_config_changed(_OldConfig, _NewConfig) ->
    {error, <<"Invalid config, no client_regex">>}.

%%--------------------------------------------------------------------
%% Working with config
%%--------------------------------------------------------------------

%% @doc
%% Efficiently get the current config.
get_config() ->
    persistent_term:get(?MODULE, #{}).

%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    PluginNameVsn = <<?PLUGIN_NAME, "-", ?PLUGIN_VSN>>,
    Config = emqx_plugin_helper:get_config(PluginNameVsn),
    ?SLOG(debug, #{
        msg => "@@name@@_init",
        config => Config
    }),
    persistent_term:put(?MODULE, Config),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({on_changed, Config}, State) ->
    persistent_term:put(?MODULE, Config),
    %% NOTE
    %% additionally handling of the config change here, i.e
    %% reestablish the connection to the database in case of host change, etc.
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    persistent_term:erase(?MODULE),
    ok.
