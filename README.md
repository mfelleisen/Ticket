## The Trains Competition 

This is a work-in-orogress repo for the Sw Dev F'2021 project. 

### Install

### Install

If you wish to inspect the code easily and experiment with it, clone the repo and then install it: 

```
$ git clone git@github.com:mfelleisen/Ticket.git
$ cd Ticket 
$ raco pkg install 
$ raco doc Ticket 
```

This last command will search for the docs of the newly installed "Ticket Project"
collection. The results are displayed in a new tab of your default browser. Follow the link
to the project and peruse the documentation, including a version of this README file. 

The `pkg install` will download and install those.

### TODO

- develop a map editor for placing points:
  - pop up a rectangle
  - populate with graph (if given)
  - allow user to add named points
  - separate window for adding/subtracting connections
  + check node name (size, letters/digits)
  + check range of segment # in JSON 

### History 

### Organization 

The repo consists of the following folders, with the links pointing to additinal "read me" files:

| directory | purpose |
|--------------------- | ------- |
| [Lib](Lib/README.md) | functionality that should probably exist in Racket's libraries | 
