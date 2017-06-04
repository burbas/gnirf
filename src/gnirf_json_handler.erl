-module(gnirf_json_handler).
-export([
         handle_msg/2
        ]).

handle_msg(Msg, State) ->
    Payload = jsx:decode(Msg),
    Command = proplists:get_value(<<"channel">>, Payload),
    handle_json(Command, Payload, State).


handle_json(undefined, _Json, _State) ->
    [{error, missing_channel}];
handle_json(_Command, _Json, _State) ->
    %% Check which MQ to send this message to
    ok.
