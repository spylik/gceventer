%% --------------------------------------------------------------------------------
%% File:    gceventer.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc
%% General purpose periodical garbage collector.
%%
%% Very good article by Fred Hebert about memory management: 
%% https://blog.heroku.com/logplex-down-the-rabbit-hole
%%
%% Idea of this app totally the same as:
%% https://github.com/heroku/logplex/blob/f1837b00fda7bb391a9a60bc4c10f51388e964ad/src/logplex_leak.erl
%% but with some advantages like more flexible api and auto-treshholding.
%%
%% This application designed only to avoid memory overflow during application tunning.
%%
%% The more clear approach is use hibernate calls and right arguments for spawned processes such as
%%
%%  {fullsweep_after, Number :: integer() >= 0} |
%%  {min_heap_size, Size :: integer() >= 0} |
%%  {min_bin_vheap_size, VSize :: integer() >= 0} |
%%  {max_heap_size, Size :: max_heap_size()} |
%% 
%% More information could be found:
%% http://erlang.org/doc/man/erlang.html#spawn_opt-4
%% http://erlang.org/faq/academic.html#idp33115216
%% @end
%% --------------------------------------------------------------------------------

-module(gceventer).
-behaviour(gen_server).

% @doc gen server is here
-behaviour(gen_server).

-define(TrashHold, 100000000).  % Size in bytes. Default value: 100mb.
-define(Heartbean, 60000).      % Hearbeat in milliseconds. Default value: 1 minute.
-define(app, ?MODULE).          % Appname same as module name

% @doc record for state
-record(state, {
    'heartbeat_freq'            :: non_neg_integer(),
    'heartbeat_tref'            :: 'undefined' | reference(),
    'treshhold' = ?TrashHold    :: pos_integer(),
    'auto_treshold' = false     :: 'false' | pos_integer()
}).


% @doc export gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc export public api
-export([
        start_link/0,
        start_link/1,
        stop/0
    ]).

% ============================ gen_server part =================================

-spec start_link() -> Result when
    Result      :: {'ok',Pid} | 'ignore' | {'error',Error},
    Pid         :: pid(),
    Error       :: {'already_started',Pid} | term().

start_link() -> 

