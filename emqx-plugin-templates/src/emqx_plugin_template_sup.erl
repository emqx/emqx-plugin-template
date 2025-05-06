{{=@@ @@=}}
-module(@@name@@_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ConfigChildSpec = #{
        id => @@name@@,
        start => {@@name@@, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [@@name@@]
    },
    SupFlags = #{
        strategy => one_for_all,
        intensity => 100,
        period => 10
    },
    {ok, {SupFlags, [ConfigChildSpec]}}.
