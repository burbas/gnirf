-module(gnirf_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {
          uuid
         }).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    Id = uuid:to_string(uuid:uuid4()),
    {reply, {text, Id}, #state{uuid = Id}}.

websocket_handle({text, Msg}, State) ->
    Response = gnirf_json_handler:handle_msg(Msg, State),
    Json = jsx:encode(Response),
    {reply, {text, Json}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

%% websocket_info({timeout, _Ref, Msg}, State) ->
%% 	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
%% 	{reply, {text, Msg}, State};
websocket_info(_Info, State) ->
	{ok, State}.
