## Milestone 3

### Release to Students

- `ForStudents`

### Files and directories for test fest 

- `xtest` is the testing script 
- `xmap` is the map-testing code
- `Tests/` is the directory of "integration tests"

### Running tests

In this directory, run 

```
$ ./xtest Tests/ ./xmap

or 

$ ./xtest ForStudents/ ./xmap
```

runs only the plainest `STDIN`/`STDOUT` tests 
found in the specified folder`in the usual manner (`<n>-in.json` is the input, and `<n>-out.json` the expected result, modulo JSON equality). 


### Note

The `map-*.json` files in 3 are generated and then used for the tests. 
