## Milestones for Sw Dev 2021 

This directory contains the integration tests for the project
milestones for [Sw Dev
2021](https://felleisen.org/matthias/4500-f21/index.html).  

| no  | integration tests for a specific project functionality   |
|---- | -------------------------------------------------------- | 
| [`3`](3/README.md) | the game-map data representation |
| [`4`](4/README.md) | a visualizations for game-maps, for debuggin |
| [`5`](5/README.md) | the legality of player-requested game actions 		|
| [`6`](6/README.md) | the naive game strategy of a mechanized player		|
| [`8`](8/README.md) | the referee's functionality of running a game 		|
| [`9`](9/README.md) | the tournament manager's functionality of running a complete tournament | 
| [`10`](10/README.md)| for turning the logical system into a distributed one   |

To run all milestone integration tests:

```
$ ./xmilestone
```

The file `get.rkt` supplies some common functionality for reading configuration files from `STDIN`. 

