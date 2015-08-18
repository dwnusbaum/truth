truth
=====
Truth is a simple Haskell program that creates a truth table given a set of variables and rules. It is currently in development.

Installation
------------

If you do not already have Haskell on your machine, you can download the Haskell platform [here](http://www.haskell.org/platform/).  To install `truth` in your home/bin directory, run the command `cabal install --prefix=$HOME --user` from the directory where you have downloaded the files.

Example Usage
-------------
`truth "A,B,C" "A->B,~C"`

The above command will create a truth table with variables A,B, and C and will test the rules A -> B and ~C. The program will output both the values that satisfy all of the rules and the full truth table to stdout.
