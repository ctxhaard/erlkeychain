-module(kc_server).

-include("kc.hrl").

-behavior(gen_server).

-export([start_link/0, load/2, is_loaded/0, first/0, set_pattern/1, next/0, get/1, put/1, delete/1, new_account/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(server_state, {accounts = [], current = 0, file_path, password, pattern}).

-define(SERVER,{global, ?MODULE}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], [] ).

%% @doc Command the server to decrypt and load FilePath
%% using Password
%% @spec (FilePath::iolist(), Pwd::iolist()) -> ok
load(FilePath, Pwd) ->
    gen_server:call(?SERVER, {load, FilePath, Pwd}).

is_loaded() ->
    gen_server:call(?SERVER, loaded).

%% @doc Get the first element of the accounts list, if any
%% @spec () -> kc_account:account() | notfound
first() ->
    gen_server:call(?SERVER, first).

%% @doc Set the pattern to filter accounts with
%% @spec (Pattern :: iolist() ) -> ok
set_pattern(Pattern) ->
    gen_server:call(?SERVER, {setpattern, Pattern}).


%% @doc Get the first element of the accounts list, if any
%% @spec () -> kc_account:account() | notfound
next() ->
    gen_server:call(?SERVER, next).

%% @doc Get the the element given its id, if any
%% @spec (integer()) -> kc_account:account() | notfound
get(AccountId) when is_number(AccountId) ->
    gen_server:call(?SERVER, {get, AccountId}).

%% @doc Add a new account or replace an existing one
%% @spec (kc_account:account()) -> ok
put(Account={account, _}) ->
    gen_server:call(?SERVER, {put, Account}).

%% @doc Delete an account identified by id
%% @spec (integer()) -> ok | notfound
delete(AccountId) when is_number(AccountId) ->
    gen_server:call(?SERVER, {delete, AccountId}).

%% @doc Prepare a new empty account
%% @spec () -> kc_account:account()
new_account() ->
    gen_server:call(?SERVER, new_account).

init(_Args) ->
    {ok, #server_state{}}.

handle_call({load, FilePath, Pwd}, _From, _State) ->
    Accounts = kc_account:load(FilePath, Pwd),
    ?LOG_DEBUG(#{ who => ?MODULE, what => "server loaded accounts", log => trace, level => debug }),
    kc_observable:notify(loaded),
    {reply, ok, #server_state{ accounts=Accounts, file_path = FilePath, password = Pwd }};

handle_call(loaded, _From, State) ->
    Loaded = if
                State#server_state.file_path =:= undefined -> false;
                true -> true
            end,
    {reply, Loaded, State };


handle_call({setpattern, Pattern}, _From, State = #server_state{ }) ->
    StateNew = State#server_state{ pattern = Pattern, current = 0},
    kc_observable:notify(loaded),
    {reply, ok, StateNew};

handle_call(first, _From, State = #server_state{ accounts=[]}) ->
    {reply, notfound, State#server_state{ current = 0 }};

handle_call(first, _From, State = #server_state{ accounts=Accounts, pattern = undefined}) ->
    [H|_] = Accounts,
    {reply, H, State#server_state{ current = 1 }};

handle_call(first, _From, State = #server_state{ accounts=Accounts, pattern = Pattern}) ->
    case kc_account:next(Accounts, 0, Pattern) of
        notfound -> {reply, notfound, State#server_state{ current = 0 }};
        {Index, Account} -> {reply, Account, State#server_state{ current = Index }}
    end;
    
handle_call(next, _From, State = #server_state{ accounts=Accounts, current=Current, pattern = undefined }) ->
    try
        Next = Current + 1,
        {reply, lists:nth(Next, Accounts), State#server_state{ current = Next }}
    catch
        error:_ -> {reply, notfound, State}
    end;

handle_call(next, _From, State = #server_state{ accounts=Accounts, current=Current, pattern=Pattern }) ->
    case kc_account:next(Accounts, Current, Pattern) of
        {Id, Account} -> 
            StateNew = State#server_state{current = Id},
            {reply, Account, StateNew};
        _ ->
            {reply, notfound, State}
    end;

handle_call({get, AccountId}, _From, State=#server_state{ accounts=Accounts}) ->
    case [Account || Account <- Accounts, kc_account:get_id(Account) =:= AccountId] of
        [] -> {reply, notfound, State};
        [H|_] -> {reply, H, State}
    end;

handle_call({put, Account = {account, Map}}, _From, State=#server_state{accounts=Accounts}) ->
    AccountId = kc_account:get_id(Account),
    case AccountId of
        0 ->
            Account1 = {account, Map#{ id := (kc_account:max_id(Accounts) + 1)}},
            StateNew = State#server_state{ accounts= [Account1 | Accounts] };
        _ ->
            AccountsClean = [X || X <- Accounts, kc_account:get_id(X) =/= AccountId],
            StateNew = State#server_state{ accounts= [Account| AccountsClean] }
    end,
    #server_state{ file_path = Pt, password =  Pw, accounts = As} = StateNew,
    kc_account:save(Pt, Pw, As),
    kc_observable:notify(loaded),
    {reply, ok, StateNew};

handle_call({delete, AccountId}, _From, State=#server_state{ accounts=Accounts}) ->
    case [Account || Account <- Accounts, kc_account:get_id(Account) =:= AccountId] of
        [] -> {reply, notfound, State};
        [H|_] ->
            StateNew = State#server_state{ accounts= Accounts -- [H], current=0 },
            ?LOG_DEBUG(#{ who => ?MODULE, what => StateNew, log => trace, level => debug }),
            #server_state{ file_path = Pt, password =  Pw, accounts = As} = StateNew,
            kc_account:save(Pt, Pw, As),
            kc_observable:notify(loaded),
            {reply, ok, StateNew }
    end;

handle_call(new_account, _From, State) ->
    {reply, kc_account:new(State#server_state.accounts), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
