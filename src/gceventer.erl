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

% @doc gen server is here
-behaviour(gen_server).

-define(ThresHold, 100000000).  % Size in bytes. Default value: 100mb.
-define(Heartbeat, 60000).      % Hearbeat in milliseconds. Default value: 1 minute.
-define(app, ?MODULE).          % Appname same as module name

% @doc type for threshold
-type threshold() :: pos_integer().

% @doc settings for autoset threshold
-type autoset_threshold() :: 'false' | pos_integer().

% @doc record for state
-record(state, {
    'heartbeat_freq'            :: non_neg_integer(),
    'heartbeat_tref'            :: 'undefined' | reference(),
    'threshold' = ?ThresHold    :: threshold(),
    'autoset_threshold' = false :: autoset_threshold()
}).
-type state() :: #state{}.


% @doc type for start options
-type start_opts() :: #{
    'heartbeat_freq' => non_neg_integer(),
    'threshold' => threshold(),
    'autoset_threshold' => autoset_threshold()

}.

% @doc export gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% @doc export public api
-export([
        start_link/0,
        start_link/1,
        stop/0,
        stop/1
    ]).

% ============================ gen_server part =================================

-spec start_link() -> Result when
    Result      :: {'ok',Pid} | 'ignore' | {'error',Error},
    Pid         :: pid(),
    Error       :: {'already_started',Pid} | term().

start_link() ->
   start_link(#{}).

-spec start_link(StartOpts) -> Result when
    StartOpts   :: start_opts(),
    Result      :: {'ok',Pid} | 'ignore' | {'error',Error},
    Pid         :: pid(),
    Error       :: {'already_started',Pid} | term().

start_link(StartOpts) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, 
        #state{
            'heartbeat_freq' = maps:get('heartbeat_freq', StartOpts, 
                application:get_env(?app, 'heartbeat_freq', ?Heartbeat)),
            'threshold' = maps:get('threshold', StartOpts, 
                application:get_env(?app, 'threshold', ?ThresHold)),
            'autoset_threshold' = maps:get('autoset_threshold', StartOpts,
                application:get_env(?app, 'autoset_threshold', false))
        },
    []).

% @doc API for stop gen_server. Default is sync call.
-spec stop() -> Result when
    Result  :: 'ok'.

stop() -> stop('sync').

% @doc API for stop gen_server.
-spec stop(Type) -> Result when
    Type    :: 'sync' | 'async',
    Result  :: 'ok'.

stop('sync') ->
    gen_server:stop(?MODULE);
stop('async') ->
    gen_server:cast(?MODULE, stop).

% @doc gen_server init section
-spec init(State) -> Result when
    State   :: state(),
    Result  :: {ok, NState},
    NState  :: state().

% @doc main init
init(State = #state{
        heartbeat_freq = Heartbeat_freq
    }) ->
    TRef = erlang:send_after(Heartbeat_freq, self(), 'heartbeat'),
    {ok, State#state{
            'heartbeat_tref' = TRef
        }
    }.

%--------------handle_call-----------------

% @doc handle_call
-spec handle_call(Message, From, State) -> Result when
    Message :: term(),
    From    :: {pid(), Tag},
    Tag     :: term(),
    State   :: state(),
    Result  :: {reply, 'ok', State}.

% @doc handle_call for unknown things
handle_call(Msg, _From, State) ->
    error_logger:warning_msg("we are in undefined handle_call with message ~p\n",[Msg]),
    {reply, 'ok', State}.
%-----------end of handle_call-------------

% @doc handle cast
-spec handle_cast(Message, State) -> Result when
    Message         :: term(),
    State           :: state(),
    Result          :: {noreply, State} | {stop, normal, State}.

% @doc handle_cast for unknown things
handle_cast(Msg, State) ->
    error_logger:warning_msg("we are in undefined handle_cast with message ~p\n",[Msg]),
    {noreply, State}.
%-----------end of handle_cast-------------


%--------------handle_info-----------------
% @doc handle_info
-spec handle_info(Message, State) -> Result when
    Message :: 'heartbeat',
    State   :: state(),
    Result  :: {noreply, State}.


% @doc hearbeat
handle_info('heartbeat', State = #state{
        heartbeat_tref = Heartbeat_tref,
        heartbeat_freq = Heartbeat_freq
    }) ->
    _ = erlang:cancel_timer(Heartbeat_tref),
    NewState = do_gc(State),
    TRef = erlang:send_after(Heartbeat_freq, self(), 'heartbeat'),
    {noreply, NewState#state{
            heartbeat_tref=TRef
        }
    };

% doc handle_info for unknown things
handle_info(Msg, State) ->
    error_logger:warning_msg("we are in undefined handle_info with message ~p\n",[Msg]),
    {noreply, State}.
%-----------end of handle_info-------------

% @doc call back for terminate (we going to cancel timer here)
-spec terminate(Reason, State) -> no_return() when
    Reason  :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State   :: state().

terminate(_Reason, #state{heartbeat_tref = Heartbeat_tref}) ->
    erlang:cancel_timer(Heartbeat_tref).

% @doc call back for code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn  :: Vsn | {down, Vsn},
    Vsn     :: term(),
    State   :: term(),
    Extra   :: term(),
    Result  :: {ok, NState},
    NState  :: term().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --------------------------- end of gen_server part ---------------------------

% =========================== other private functions ==========================

% @doc do garbace collection routine
-spec do_gc(State) -> NState when
    State   :: state(),
    NState  :: state().

do_gc(State = #state{
        'threshold' = Threshold,
        'autoset_threshold' = AutoSet
    }) ->
        BeforeGc = erlang:memory(total),
        case BeforeGc >= Threshold of
            true when AutoSet =:= false ->
                [erlang:garbage_collect(Pid) || Pid <- processes()],
                After = erlang:memory(total),
                error_logger:info_msg("GC stat: Before: ~p; after: ~p",[BeforeGc,After]),
                State;
            true ->
                [erlang:garbage_collect(Pid) || Pid <- processes()],
                After = erlang:memory(total),
                error_logger:info_msg("GC stat: Before: ~p; after: ~p",[BeforeGc,After]),
                State#state{
                    'threshold' = After + AutoSet
                };
            false -> 
                State
        end.
