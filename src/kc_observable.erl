-module(kc_observable).
-behaviour(gen_event).

-include("kc.hrl").

-export([start_link/0, notify/1, add_observer/1, remove_handler/1]).
-export([init/1, handle_event/2, handle_call/2]).

-define(SERVER, {global, ?MODULE}).

%% ----------------------------------------------------------------------------
%%  Public interface
%% ----------------------------------------------------------------------------
start_link() ->
    Ret = gen_event:start_link(?SERVER, []),
    gen_event:add_handler(?SERVER, kc_observable,[]),
    Ret.

notify(Event) ->
    ?LOG_DEBUG(#{ who => ?MODULE, what => "received notification", log => trace, level => debug }),
    gen_event:notify(?SERVER, Event).

add_observer(Function) ->
    gen_event:notify(?SERVER, {add, Function}).

remove_handler(Function) ->
    gen_event:notify(?SERVER, {remove, Function}).

%% ----------------------------------------------------------------------------
%%  gen_event behavior
%% ----------------------------------------------------------------------------
init(_Args) ->
    ?LOG_DEBUG(#{ who => ?MODULE, what => "init observer", log => trace, level => debug }),
    {ok, []}.

handle_call(_Request, State) ->
    {ok, noop, State}.

handle_event({add, Function}, State) ->
    ?LOG_DEBUG(#{ who => ?MODULE, what => "adding observer", log => trace, level => debug }),
    {ok, [Function|State]};

handle_event({remove, Function}, State) ->
    ?LOG_DEBUG(#{ who => ?MODULE, what => "removing observer", log => trace, level => debug }),
    {ok, State -- [Function]};

handle_event(Event, State) ->
    lists:foreach(fun (F) -> 
        ?LOG_DEBUG(#{ who => ?MODULE, what => "fan out notification", log => trace, level => debug }),
        F(Event)
    end, State),
    {ok, State}.
