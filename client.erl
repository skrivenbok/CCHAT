-module(client).
-export([initial_state/3, handle/2]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    channels,
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.

% Added channels to the record.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        channels = [],
        server = ServerAtom
    }.

% handle join, we send a request to the server to join.
handle(St, {join, Channel}) ->
    case catch genserver:request(St#client_st.server, {join, self(), Channel}) of 
        ok -> 
            NewChannelsList = [Channel | St#client_st.channels],
            {reply, ok, St#client_st{channels = NewChannelsList}};
        {'EXIT', "Timeout"} -> 
            {reply,{error, server_not_reached, "Server non-responsive"},St};
        {'EXIT', {badarg,_}} -> 
            {reply,{error, server_not_reached, "Server not existing"},St};	
         _ -> 
            {reply,{error, user_already_joined, "User already joined"}, St}                     
    end;


% handle leave, we send a request to the channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of
        true ->
            Reply = (try genserver:request(list_to_atom(Channel), {leave, self()}) of
                        Response -> Response
                    catch
                        error:_ -> {error, server_not_reached, "Server wasnt reachable"}
                    end),
            NewChannelsList = lists:delete(Channel, St#client_st.channels),
            {reply, Reply, St#client_st{channels=NewChannelsList}};
        false ->
            {reply, {error, user_not_joined, "Not member of channels"}, St}
    end;

%handle message_sendm we send a request to the channel
handle(St, {message_send, Channel, MSG}) ->
    case whereis(list_to_atom(Channel)) of
        undefined ->
            {reply,{error, server_not_reached, "_"}, St};
        _Else ->
            Reply = (try genserver:request(list_to_atom(Channel), {message_send, self(), St#client_st.nick, MSG}) of
                        Response -> Response
                    catch 
                        error:_ -> {error, server_not_reached, "Server wasnt reachable"}
                    end),
            {reply, Reply, St}
end;
    
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.


