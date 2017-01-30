-module(gceventer_sup).

% supervisor is here
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> Result when
    Result :: 'ignore' | {'error',_} | {'ok',pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) -> Result when
    Result :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

init([]) ->
    RestartStrategy = {one_for_one, 10, 10}, 

    GCEventer = {
        gceventer,                              % ID
        {gceventer, start_link, []},            % Start
        permanent,                              % Children restart strategy (temporary - if they die, they should not be restarted)
        5000,                                   % Shutdown strategy
        worker,                                 % Child can be supervisor or worker
        [gceventer]                             % Option lists the modules that this process depends on
    },

    Childrens = [GCEventer],                    % Mapping paraments defined in Server to childrens. We can specify many childrens
    {ok, {RestartStrategy, Childrens}}.
