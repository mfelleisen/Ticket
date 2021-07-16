## The Trains Competition 

This is a work-in-orogress repo for the Sw Dev F'2021 project. 

### Install

### Install

If you wish to inspect the code easily and experiment with it, clone the repo and then install it: 

```
$ git clone git@github.com:mfelleisen/Ticket.git
$ cd Ticket 
$ raco pkg install 
$ raco doc Trains 
```

This last command will search for the docs of the newly installed "Ticket Project"
collection. The results are displayed in a new tab of your default browser. Follow the link
to the project and peruse the documentation, including a version of this README file. 

The `pkg install` will download and install those.

### TODO

- develop a data representation for maps 
  - consider efficient: access to connections and computation of connectivity
  
### Design History

- A city can connect to many other cities. Each bundle of connections can
  use any or all of the four colors. 
  - Mapping from City String to City String to Color String to Segment#
  - this would enforce that between two cities each color is used at most once 

  The Map guarantees each connection is specified at most once) to order the
  connecting cities. Player strategies need access to all possible connections from one city to its
  neighbors so a list is better than an enforced invariant. 

  A data structure that enforces the logical invariant can be used for JSON
  but it's not what we want for planning paths---which is a frequent operation. 

  Once the map is fixed, the constraints are guaranteed.
  BUT, every connection some player has taken is no longer available, so it should be removed.
  fast lookup:
  - use a list first
  - and when it gets large use a hash of (connection ..) struct ?


- develop a map editor for placing points:
  - pop up a rectangle
  - populate with JSON graph (if given)
  - allow user to add named points
  - separate window for adding/subtracting connections
  - deliver JSON representation with all essential information 
  - check node name (size, letters/digits)
  - check range of segment # in JSON 


### Organization 

The repo consists of the following folders, with the links pointing to additinal "read me" files:

| directory | purpose |
|--------------------- | ------- |
| [Common](Common/README.md) | the common ontology: understanding the communication between game server and client player | 
| [Docs](Docs/README.md) | the source of the scribble documentation | 
| [Editor](Editor/README.md) | editing maps: a plaything for now | 
| [Lib](Lib/README.md) | functionality that should probably exist in Racket's libraries | 
| [Resources](Resources/README.md) | pictures | 
