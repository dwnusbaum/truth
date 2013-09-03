truth
=====
Truth is a simple Haskell program that creates a truth table given a set of variables and rules. It is currently in development.

Example Usage
-------------
truth "A;B;C;A->B;~C" truthtable.csv
The above command will create a truth table with variables A,B, and C and will test the rules A -> B and ~C. The program will display the values that satisfy all of the rules, and will store the truth table in the file truthtable.csv. If no filename is specified it defaults to truth.csv. 
