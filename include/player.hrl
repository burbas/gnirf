-record(player, {
          uuid :: binary(),
          games = [] :: [{binary(), binary()}],
          chat_rooms = [] :: [{binary(), binary()}]
         }).
