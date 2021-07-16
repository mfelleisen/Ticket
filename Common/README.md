## The Common Ontology

the common ontology: understanding the communication between game server and client player

The [`Common`](Common/) directory contains the materials that define
the common ontology between external players and the server.

### The APIs

- `player-interface` describes how the service side interacts with players 
- `rules` a data representation for both checking the legality of turns and planning them
- `state` a data representation of game states: board, orderig of players, knowledge about players 
- `internal` a representation of the knowledge about the players currently in the game 
- `board` a representation of the game board
- `fish` basic constants for the fish tiles
- `penguin` basic constants for the players' avatars 

### The Protocols 

The API assumes the following protocols for the collaboration of the
`Admin` components and `Players`:

- `protocol-start-tournament` ~~ signing up for the tournament
- `protocol-run-tournament` ~~ starting  the round of games 
- `protocol-launch-game` ~~ launching an individual game, set-up phase 
- `protocol-play-turn` ~~ playing a turn during an individual game
- `protocol-end-tournament` ~~ ending the tournament 

A participant has access to this folder because it fully specifies the
rules of engagement.

**Note** The protocol part reveals too much about the internal
organization but none of these revelations should affect safety,
security, or fairness.

| file | purpose |
|--------------------- | ------- |
| [basic-constants.rkt](basic-constants.rkt) | ovide | 
| [board.rkt](board.rkt) | a bi-directional graph representation of the railroad map | 
| [node-serialize.rkt](node-serialize.rkt) | serializing nodes from the map editor | 
| [serialize.rkt](serialize.rkt) | serialization of board maps to JSON | 
| [state.rkt](state.rkt) | --------------------------------------------------------------------------------------------------- | 
