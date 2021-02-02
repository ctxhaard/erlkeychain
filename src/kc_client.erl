-module(kc_client).
-behavior(gen_statem).

-export([start_link/1]).
%% gen_statem callback functions
-export([init/1, callback_mode/0, terminate/3]).
%% state machine functions
-export([password_prompt/3, list_accounts/3, show_account/3, edit_account/3]).
-include("kc.hrl").


-define(NAME, ?MODULE).

start_link(Args) ->
  gen_statem:start_link({local, ?NAME}, ?MODULE, Args, []).

init(Args) ->
  {ok, password_prompt, []}.

callback_mode() ->
  state_functions.

terminate(Reason, State, Data) ->
  ok.

password_prompt(enter, OldState, Data) ->
  {keep_state, Data};
password_prompt(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

list_accounts(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

show_account(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.

edit_account(EventType, EventContent, Data) ->
  {next_state, list_accounts, Data}.