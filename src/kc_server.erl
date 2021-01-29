-module(kc_server).
-behavior(gen_server).

-export([start_link/0, load/2, first/0, next/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {accounts = [], current = 0}).
-define(SERVER,?MODULE).

%% gen_server behavior
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [] ).

%% @doc Command the server to decrypt and load FilePath
%% using Password
%% @spec (FilePath::iolist(), Pwd::iolist() -> ok
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

% put()
% get()
% delete()

init(_Args) ->
    {ok, #state{}}.

handle_call({load, FilePath, Pwd}, _From, _State) ->
    Accounts = kc_account:load(FilePath, Pwd),
    {reply, ok, #state{ accounts=Accounts}};

handle_call(first, _From, State = #state{ accounts=Accounts}) when Accounts =/= [] ->
    {reply, hd(Accounts), State#state{ current = 1 }};

handle_call(first, _From, State = #state{ accounts=Accounts}) when Accounts =:= [] ->
    {reply, notfound, State#state{ current = 0 }};

handle_call(next, _From, State = #state{ accounts=Accounts, current=Current}) ->
    try
        Next = Current + 1,
        {reply, lists:nth(Next, Accounts), State#state{ current = Next }}
    catch
        _ -> {reply, notfound, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.