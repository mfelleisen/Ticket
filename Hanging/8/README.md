## Milestone 6

### Release to Students

- `ForStudents`

### Files and directories for test fest 

- `xtest` is the testing script 
- `xref` is the map-testing code
- `Tests/` is the directory of "integration tests"

### Running tests

In this directory, run 

```
$ ./xtest Tests/ ./xref

or 

$ ./xtest ForStudents/ ./xref
```

runs only the plainest `STDIN`/`STDOUT` tests 
found in the specified folder`in the usual manner (`<n>-in.json` is the input, and `<n>-out.json` the expected result, modulo JSON equality). 



