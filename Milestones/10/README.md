## Milestone 10

### Files for test fest 

- `xtest` is the testing script 

- `xserver` ~~ on the CMD LINE it is invoked with the port number that it listens to; 
  - it consumes the configuration for a tournament from `STDIN`; 
  - it sends the result of the tournament to `STDOUT`
  
- `xclients` ~~ on the CMD LINE, it is invoked with the port number and, optionally, with an IP address; 
  - it consumes the configuration for a tournament from STDIN;
  - its output is ignored and there shouldn't be any 

Running `xtest` will create the file

- `port-starter-file.rktd`

which records the port last used by the server --- just in case something goes wrong with the TCP ports. 
It is safe to delete this file after a run. 

Finally, the directory comes with 

- `run-server-client.rkt` which collects the commonalities of `xserver` and `xclients`

### Test Directories:

- `ClientTests` configure my clients to throw at their server
- `ServerTests` configure their clients to run with my server 
- `BounsTests` configure my clients to send bad JSON to their server, in addition to playing and cheating 

### Running the required tests

**Note** If you took SwDev 2021, Cameron adjusted the test inputs so
  that your code would still function properly even if it parsed the
  (unnecessary parts of the) configuration inputs for correctness. If
  you wish to run these tests for your existing code, you will need to
  work around this problem manually or via an editing script. 

`xtest` should be run twice. 

The first run tests their clients: 

```
$ ./xtest ClientTests/ PathTo_THEIR_Client PathTo_MY_server 
```

The `ClientTests` are simple configurations for their players, 
which in the context of my server. It checks whether they can run games if everything goes well. 


The second run tests their server: 

```
$ ./xtest ServerTests/ PathTo_MY_Client PathTo_THEIR_server
```

The `ServerTests` are configurations for my players, some that cheat and some that misbehave. The point is to check whether their server holds up. At least for the basics (good ones and cheaters) it should hold up. 

### Running the bonus tests

Run 

```
$ ./xtest BonusTests/ PathToMy_XBonus_Clients PathTo_THEIR_server
```

