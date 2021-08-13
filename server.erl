-module(server).
-export([start/1, stop/1, serverhandle/2]).

-record(server_st, {channels, nicks}).
%% list of all channels registered so far

initial_state() ->
    #server_st {
        channels = [],
        nicks = []
    }.

%serverhandle join, looking for registered Channels and either contacting the channel if: 
% registered, else: starting a new channel    
serverhandle(St, {join, Pid, Channel}) ->
    case lists:member(Channel, St#server_st.channels) of
        true -> 
            Response = genserver:request(list_to_atom(Channel),{join, Pid}),
                {reply, Response, St};
                       
        false ->
            ChanneList = [Channel | St#server_st.channels],
            genserver:start(list_to_atom(Channel), channel:channel_st(Channel), fun channel:handle/2),
            try genserver:request(list_to_atom(Channel),{join, Pid}) of
                Response ->
                    {reply, Response, St#server_st{channels = ChanneList}}
            catch
                error:_ ->
                    {reply,{error, server_not_reached, "server nwasnt reachable"}, St}
            end      
    end;
    
            
serverhandle(St, stop) ->
    ChanneList = St#server_st.channels,
	[genserver:stop(list_to_atom(Channel)) || Channel <- ChanneList],
    {reply, ok, St}; 

%if it is som random pattern that reaches server. Dont know if this is necessary
serverhandle(St, _) ->
        {reply, {error, not_implemented, "pattern matchning against some random pattern"}, St}. 
      

% Stop the server process registered to the given name,
% together with any other associated processes
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun serverhandle/2). 

stop(ServerAtom) ->  
    genserver:request(ServerAtom, stop),
    genserver:stop(ServerAtom).



    
    
