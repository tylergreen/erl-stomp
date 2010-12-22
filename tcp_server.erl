% tcp frontend to stomp server

-module(tcp_server).
-behavior(gen_server).
-compile(export_all).

%% api

-export([
	 start_link/1,
	 start_link/0,
	 get_count/0,
	 stop/0
	]).

%% callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

% API

start_link() ->
    start_link(?DEFAULT_PORT).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

get_count() ->
    gen_server:call(?SERVER, get_count).

stop() ->
    gen_server:cast(?SERVER, stop).

% gen_server callbacks

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply,State#state{request_count = RequestCount + 1 }};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

handle_call(get_count, _From, State) ->
    {reply,
	{ok, State#state.request_count},
	State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

do_rpc(Socket, RawData) ->
    M = lists:filter(fun(X) -> X /= 0 end, RawData), % deal with this better
    {Command, Headers} = parse_data(M),
    do_command(Command, Headers, Socket).

do_command(Command, Headers, Socket) ->
    Valid = lists:member(Command, ["CONNECT", "SUBSCRIBE", "UNSUBSCRIBE, SEND"]), % actually take action if sent a bad command
    F = list_to_atom(string:to_lower(Command)),
    F(Headers, Socket).
    
% should figure login out at some point
connect(_Headers, Socket) ->
    % no idea what this should actually do
    io:format('received connection attempt'),
    gen_tcp:send(Socket, << "CONNECTED",$\n,"session: 1", $\n,$\n, 0 >>).

subscribe(Headers, Socket) ->
    Dest = lookup("destination", Headers),
    qmanager:subscribe(Dest, Socket).

% frame parsing

% probably need some error checking here
% make sure no security risk is introduced from Command
parse_data(RawData) ->
    [Command | RawHeaders] = splitall($\n, RawData),
    Headers = lists:map(fun(H) -> spliton($:, H) end,
			RawHeaders),  % return tuples instead of lols?
    {Command, Headers}.

spliton(Delim, Xs) ->
    case lists:splitwith(fun(X) -> X /= Delim end, Xs) of
	{F, [_|T] } -> {F, T};
	{F, []} -> {F, []}   %  no delimeter is encountered
    end.

% write tests
splitall(Delim, Xs) ->
    case spliton(Delim, Xs) of 
	{[], []} -> [];
	{As, []} -> [As];
	{[], Bs} -> splitall(Delim, Bs);
	{As, Bs} -> [As | splitall(Delim, Bs)]
    end.

% convenince for parsing headers
lookup(Elem, Xs) ->
    lists:keyfind(Elem, 1, Xs).

