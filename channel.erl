-module(channel).
-export([handle/2, channel_st/1]).

%channel and list of nicks in channel
-record(channel_st, {
    channel,
    nicks
}).

channel_st(Channel) ->
        #channel_st { 
           channel = Channel,
           nicks = []
        }.

handle(St,{join, Pid}) ->
        case lists:member(Pid, St#channel_st.nicks) of 
                true ->
                    {reply, {error, user_already_joined, "user already joined"}, St};
                false ->
                    NickList = [Pid| St#channel_st.nicks],
                    {reply, ok, St#channel_st{nicks = NickList}}        
        end;


handle(St, {leave, Pid}) ->
    case lists:member(Pid,St#channel_st.nicks) of
        true -> 
            NickList = lists:delete(Pid, St#channel_st.nicks),
            {reply, ok, St#channel_st{nicks=NickList}};
        false -> 
            {reply, {error, user_not_joined, "user not joined"}, St}
    end;


handle(St, {message_send, Pid, Nick, Msg}) ->
    case lists:member(Pid, St#channel_st.nicks) of
        true -> 
            NewNicks = lists:delete(Pid, St#channel_st.nicks),
            Data = {request, self(), make_ref(), {message_receive, St#channel_st.channel, Nick, Msg}},
            lists:foreach((fun(Nicks) -> Nicks ! Data end), NewNicks),
            {reply, ok, St};
        false ->
            {reply,{error,user_not_joined, "user not joined"}, St}
end;

%if it is som random pattern that reaches server. Dont know if this is necessary

handle(St, _) ->
    {reply, {error, not_implemented, "Channel cannot handle this request!"}, St}.    
