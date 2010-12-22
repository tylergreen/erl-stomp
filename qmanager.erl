% stomp server queue manager
-module(qmanager).
-behaviour(gen_server).

-record(user, {connection, ack}).

%% Stomp Protocol API

% users are sockets
subscribe(User, Dest) ->
    gen_server:call(?MODULE, {subscribe, Dest, User}).

unsubscribe(User, Dest) ->
        gen_server:call(?MODULE, {unsubscribe, Dest, User}).

send(Dest, Message) -> 
    gen_server:call(?MODULE, {send, Dest, Message} ).

%% Service API

%% Callback Functions

init() ->
    ets:new(qstore, [named_table]),
    ets:new(queues, [named_table]),
    {ok, null}.

terminate(Reason, State) ->
    stuff.

handle_cast(stop, State) ->
    stuff.

% incoming message
handle_cast({send, Dest, Frame}, _From, State) ->
    in(messageq, Dest, Frame),    
    {reply, ok, null}.

handle_cast({subscribe, Dest, User}, From, State) ->
    in(userq, Dest, User),
    send_destination_backlog(Dest, User),
    {reply, ok, null}.

% want to hide all the details this exposes
handle_cast({unsubscribe, Dest, Connection}, From, State) ->
    Q = ets:lookup(userq, Dest),
    Q1 = queue:filter(fun(X) -> X /= Connection end, Q), % user is now a record
    case queue:is_empty(Q1) of
	true ->	ets:delete(userq, Dest);
	false -> ets:insert(userq, {Dest, Q1})
    end
    {reply, ok, null}.

%handle_cast({begin, }) -> .

%handle_cast({commit, }) ->     .

%handle_cast({abort, }) -> .

%handle_cast({ack, }).

%handle_cast({disconnect}) -> .

%% internal functions

send_destination_backlog(Dest, #user{conn = C, ack=Ack}) ->
    case Ack of
	true ->
	    Frame = out(userq, Dest)
	    send_to_user(Frame, User);
        false ->
	    send_all(messageq, Dest, User)  % send_all messages in a queue to a user
    end
end

% queueing api

in(Q, Dest, X) ->
    ets:insert(Q, {Dest, queues:in(X,Q)}),
    ok.

out(Q, Dest) ->
    InnerQ = ets:lookup(Q, Dest),
    {{value, X}, InnerQ2} = queue:out(InnerQ),
    ets:insert(Q, {Dest, InnerQ2}),
    X.








    

     
