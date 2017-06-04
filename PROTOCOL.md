# PROTOCOL FOR GNIRF

## Chat

### Sending actions to a subject

| channel     | subject   | ... |
| ----------- | --------- | --- |
| "chat_room" | room/nick | ... |

This sends a command to the chat-application with a set subject. The subject can either be a chat-room or a nickname.



### Game

| channel    | ident          | subject    | ... |
| ---------- | -------------- | ---------- | ... |
| game       | id of the game | game-token | ... |

Sends a request to a specific game (Identified by the global "id of the game" and a specific game-token. If the game-token is missing Gnirf will threat this as a match-making request.
