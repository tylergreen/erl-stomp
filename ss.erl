-module(ss).
-behavior(gen_server).

% API
-export([
	 start_link/1,
	 start_link/0,
	 get_count/0,
	 stop/0
	]).

% Callbacks

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

% API

start_link() ->
    start_link(?DEFAULT_PORT).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?SERVER, stop).

% Callbacks

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.  % 0 is a timeout

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    ss:rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


mystart(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, false}]),
    {ok, RawData} = gen_tcp:recv(LSock,0),
    parse(RawData).

parse(RawData) ->
    [Command | X] = string:tokens(RawData, "\n"),
    case Command of
	"CONNECT" ->
	    {connect, X};
	"DISCONNECT" ->
	    {disconnect, X};
	"SUBSCRIBE" ->
	    {subscribe, X};
	"SEND" ->
	    {send, X} ;
	"UNSUBSRIBE" ->
	    {unsubscribe, X}
    end.

spliton(Delim, Xs) ->
    case lists:splitwith(fun(X) -> X /= Delim end, Xs) of 
	{[], []} -> [];
	{As, []} -> [As];
	{[], [_|Bs]} -> spliton(Delim, Bs);
	{As, [_|Bs]} -> [As | spliton(Delim, Bs)]
    end.



				       



	






    

