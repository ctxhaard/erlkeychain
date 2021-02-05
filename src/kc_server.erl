-module(kc_server).

-include("kc.hrl").

-behavior(gen_server).

-export([start_link/0, load/2, first/0, next/0, get/1, put/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(server_state, {accounts = [], current = 0, file_path, password}).

-define(SERVER,?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [] ).

%% @doc Command the server to decrypt and load FilePath
%% using Password
%% @spec (FilePath::iolist(), Pwd::iolist()) -> ok
load(FilePath, Pwd) ->
    gen_server:call(?SERVER, {load, FilePath, Pwd}).

%% @doc Get the first element of the accounts list, if any
%% @spec () -> kc_account:account() | notfound
first() ->
    gen_server:call(?SERVER, first).

%% @doc Get the first element of the accounts list, if any
%% @spec () -> kc_account:account() | notfound
next() ->
    gen_server:call(?SERVER, next).

%% @doc Get the the element given its id, if any
%% @spec (AccountId) -> kc_account:account() | notfound
get(AccountId) ->
    gen_server:call(?SERVER, {get, AccountId}).

%% @doc Add a new account or replace an existing one
%% @spec (kc_account:account()) -> ok
put(Account={account, _}) ->
    gen_server:call(?SERVER, {put, Account}).

%% @doc Delete an account identified by id
%% @spec (integer()) -> ok | notfound
delete(AccountId) ->
    gen_server:call(?SERVER, {delete, AccountId}).

init(_Args) ->
    {ok, #server_state{}}.

handle_call({load, FilePath, Pwd}, _From, _State) ->
    Accounts = kc_account:load(FilePath, Pwd),
    ?LOG_DEBUG(#{ who => ?MODULE, what => "server loaded accounts", log => trace, level => debug }),
    kc_ncurses:updated(accounts),
    kc_client:loaded_event(),
    {reply, ok, #server_state{ accounts=Accounts, file_path = FilePath, password = Pwd }};

handle_call(first, _From, State = #server_state{ accounts=Accounts}) when Accounts =/= [] ->
    {reply, hd(Accounts), State#server_state{ current = 1 }};

handle_call(first, _From, State = #server_state{ accounts=Accounts}) when Accounts =:= [] ->
    {reply, notfound, State#server_state{ current = 0 }};

handle_call(next, _From, State = #server_state{ accounts=Accounts, current=Current}) ->
    try
        Next = Current + 1,
        {reply, lists:nth(Next, Accounts), State#server_state{ current = Next }}
    catch
        error:_ -> {reply, notfound, State}
    end;

handle_call({get, AccountId}, _From, State=#server_state{ accounts=Accounts}) ->
    case [Account || Account <- Accounts, kc_account:get_id(Account) =:= AccountId] of
        [] -> {reply, notfound, State};
        [H|_] -> {reply, H, State}
    end;

handle_call({put, Account}, _From, State=#server_state{accounts=Accounts}) ->
    AccountId = kc_account:get_id(Account),
    AccountsClean = [X || X <- Accounts, kc_account:get_id(X) =/= AccountId],
    StateNew = State#server_state{ accounts= [Account| AccountsClean] },
    {reply, ok, StateNew};

handle_call({delete, AccountId}, _From, State=#server_state{ accounts=Accounts}) ->
    case [Account || Account <- Accounts, kc_account:get_id(Account) =:= AccountId] of
        [] -> {reply, notfound, State};
        [H|_] ->
            StateNew = State#server_state{ accounts= Accounts -- [H], current=0 },
            ?LOG_DEBUG(#{ who => ?MODULE, what => StateNew, log => trace, level => debug }),
            kc_account:save(StateNew#server_state.file_path, StateNew#server_state.password, StateNew#server_state.accounts),
            {reply, ok, StateNew }
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
