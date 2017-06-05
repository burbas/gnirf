-module(gnirf_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include("../include/player.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
    %% Generate a UUID so we can identify this particular user
    Id = erlang:list_to_binary(uuid:to_string(uuid:uuid4())),
    %% Setup connection to RMQ
    MQSettings = mq_init(Id),
    {reply, {text, Id}, #player{uuid = Id,
                                mq_settings = MQSettings
                               }}.

websocket_handle({text, Msg}, State) ->
    {Response, NewState} = gnirf_json_handler:handle_msg(Msg, State),
    Json = jsx:encode(Response),
    {reply, {text, Json}, NewState};

websocket_handle(Data, State) ->
    io:format("Got unhandled data in websocket_handle: ~p~n", [Data]),
    {ok, State}.

%% websocket_info({timeout, _Ref, Msg}, State) ->
%% 	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
%% 	{reply, {text, Msg}, State};
websocket_info({#'basic.deliver'{delivery_tag = _Tag}, #amqp_msg{payload = Payload}}, State) ->
    io:format("Got RMQ message: ~p~n", [Payload]),
    {reply, {text, Payload}, State};
websocket_info(Info, State) ->
    io:format("Got undhandled data in websocket_info: ~p~n", [Info]),
    {ok, State}.


mq_publish(RoutingKey, Message, State = #player{mq_settings = #{channel := Channel,
                                                                exchange := Exchange,
                                                                queue := Queue}}) ->
    Publish = #'basic.publish'{exchange = Exchange, routing_key = Queue},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Message}).

mq_init(Id) when is_binary(Id) ->
    Username = application:get_env(gnirf, mq_username, <<"rabbitmq">>),
    Password = application:get_env(gnirf, mq_password, <<"rabbitmq">>),
    VirtualHost = application:get_env(gnirf, mq_vhost, <<"/">>),
    Host = application:get_env(gnirf, mq_host, <<"rabbit">>),
    Port = application:get_env(gnirf, mq_port, 5672),
    ChannelMax = application:get_env(gnirf, mq_max_channel, 1024),
    Exchange = application:get_env(gnirf, mq_exchange, <<"gnirf">>),

    {ok, Connection} = amqp_connection:start(#amqp_params_network{
                                               username = Username,
                                               password = Password,
                                               virtual_host = VirtualHost,
                                               %host = Host,
                                               port = Port,
                                               channel_max = ChannelMax}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'queue.declare_ok'{queue = Queue}
        = amqp_channel:call(Channel, #'queue.declare'{auto_delete = true}),
    Binding = #'queue.bind'{queue = Queue,
                            exchange = Exchange,
                            routing_key = <<"user_event.", Id/binary, ".#">>},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} =
        amqp_channel:call(Channel, Sub),

    %% Create a map and store the information within it
    #{connection => Connection,
      channel => Channel,
      bindings => Binding,
      tag => Tag}.


mq_stop(#{channel := Channel, connection := Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.
