## The Ticket-to-Ride Project for Sw Dev 2021 

The repository is a fully functioning game framework.  It is both an
educational and a research project. The code is derived from the Fall
2021 version of the "Software Development" course.  The "trace" branch
is an exploration of trace contracts "in the wild."

### Install

If you wish to inspect the code easily and experiment with it, clone the repo and then install it: 

```
$ git clone git@github.com:mfelleisen/Ticket.git
$ cd Ticket 
$ raco pkg install 
```

This installs the package as a `collect` called `Trains`.

The package depends on several other repos, including Cameron Moy's
`trace` contracts. The `pkg install` will download and install those.

### The Idea 

"Ticket to Ride" is a board game for players eight and older. 

This repository is a framework for programming competitive "Ticket" players, a variant
of the original game.  Participants will design automated players that run on their
desktops and connect to a (remote) "Ticket" server. This server will run "knock out"
tournaments of games consisting of two to eight players each. Any misbehavior of a
player---non-responsiveness due to bugs or cheating---results in immediate
termination. So the competition is first about delivering robust code and second about
writing great strategies. 

```
 +----------------------------+                           +----------------------------+
 | Client                     |                           | Server                     |
 +----------------------------+                           +----------------------------+
 | player mechanism           |                           | tournament manager         |
 | strategy                   | relies on      relies on  | referees, its game state   |
 | GUI mechanism for people?  |-----------+  +------------| observers                  |
 +----------------------------+           |  |            +----------------------------+
                                          |  |
                                          v  v
                 +---------------------------------------------------------+
                 | the common ontology of Clients and Server               |
                 +---------------------------------------------------------+
                 | player interface and protocols                          |
                 | the player's game state: its possessions,               |
                 |     plus knowledge about others                         |
                 | basic game pieces and constants, specifically maps      |
                 +---------------------------------------------------------+
```

### The Basic Idea

The [Software Dev course web page](https://felleisen.org/matthias/4500-f21/) spells
out the rules of the game, a production plan, and specific milestones (though they are
tuned toward pedagogic goals).

See the "Projects Milestones" tab at the bottom on the left. 

### What You Can Do

Without any coding, you can use this repo to

- observe "AI" games, or
- play interactive games.

Using Racket, you can use this repo to quickly 

- try out new game strategies
- develop different forms of players.

Using any programming language, you can use this repo to

- test remote clients ("AI players") locally 
- stage local and/or remote tournaments. 

The communication between server and clients is TCP-based, using JSON-formatted messages.

### The Common Ontology

For participants to connect to the server, they need to understand the interaction protocol, which
specify the sequence of remote calls and their signatures.  While each message is just a piece JSON
data, the signatures interpret these pieces of data in the context of the game.

Since even a detailed interpretation may leave questions about their meaning with respect to the
rules of the game. the repository's `Common` directory publishes the code that interprets the remote
calls with respect to the server and the sample player in this repository.

### Organization 

The repo consists of the following folders, with the links pointing to additinal "read me" files:

| directory | purpose |
|--------------------- | ------- |
| [Admin](Admin/README.md) | the logical tournament manager, referee, and game observer (todo) | 
| [Client](Client/README.md) | the client side of a distributed system for Ticket to Ride | 
| [Common](Common/README.md) | the common ontology: understanding the communication between game server and client player | 
| [Editor](Editor/README.md) | editing maps: a plaything for now | 
| [Lib](Lib/README.md) | functionality that should probably exist in Racket's libraries | 
| [Player](Player/README.md) | the logical sample player | 
| [Remote](Remote/README.md) | the remote proxies for the server and clients of a distributed Ticket to Ride tournament | 
| [Resources](Resources/README.md) | pictures | 
| [Server](Server/README.md) | the server part of a distributed tournament system for Ticket to Ride | 
