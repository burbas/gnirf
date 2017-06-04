-module(gnirf_json_handler).
-export([
         handle_msg/2
        ]).

-include("../include/player.hrl").

handle_msg(Msg, State) ->
    try jsx:decode(Msg) of
        Payload ->
            Command = proplists:get_value(<<"channel">>, Payload),
            handle_json(Command, Payload, State)
    catch
        _:_ ->
            {[{error, invalid_json}], State}
    end.

%% Handle the case when we don't have a channel given
handle_json(undefined, _Json, State) ->
    {[{error, missing_channel}], State};

%% Handling events related to chat rooms
handle_json(<<"chat_room">>, Json, State) ->
    case proplists:get_value(<<"subject">>, Json) of
        undefined ->
            {[{error, missing_subject}], State};
        Subject ->
            _RoutingKey = <<"chat_room.", Subject/binary>>,
            % Get this out there
            Response = ok,
            {Response, State}
    end;

%% Handle events related to games
handle_json(<<"game">>, Json, State) ->
    case proplists:get_value(<<"ident">>, Json) of
        undefined ->
            {[{error, missing_game_ident}], State};
        GameIdent ->
            RoutingKey =
                case proplists:get_value(<<"subject">>, Json) of
                    undefined ->
                        <<"game.", GameIdent/binary, ".matchmaking">>;
                    Subject ->
                        <<"game.", GameIdent/binary, Subject/binary>>
                end,
            %% Use the Routingkey send out the request
            io:format("RoutingKey: ~p~n", [RoutingKey]),
            Response = ok,
            {Response, State}
    end;

%% Other messages are ignored for now
handle_json(_Command, _Json, State) ->
    %% Check which MQ to send this message to
    {ok, State}.
