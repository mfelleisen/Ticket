## The Common Ontology

the common ontology: understanding the communication between game server and client player

The [`Common`](Common/) directory contains the materials that define
the common ontology between external players and the server.

### The APIs

- `player-interface` describes how the service side interacts with players 

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
| [map-serialize.rkt](map-serialize.rkt) | serialization of board maps to JSON | 
| [map.rkt](map.rkt) | a data representation of the railroad map | 
| [player-interface.rkt](player-interface.rkt) | the player interface | 
| [state-serialize.rkt](state-serialize.rkt) | ovide | 
| [state.rkt](state.rkt) | representation of a player's knowledge about the game | 
