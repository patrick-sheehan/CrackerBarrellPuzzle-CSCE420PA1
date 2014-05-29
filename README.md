CrackerBarrellPuzzle-CSCE420PA1
===============================
Programming Assignment #1 - Cracker Barrel Game Solver
Patrick Sheehan
CSCE 420
22 January 2014
Sources: haskell.org, stackoverflow.com, piazza,
         http://www.danobrien.ws/PegBoard.html
         
This program was written in Haskell. Generally, a board is evaluated
to see if there are any possible moves that can be made. If there
aren’t any, it is checked to see if it is a success (one peg anywhere,
and one peg in a corner). If there are possible moves, evaluate all of
them in this recursive pattern. A board is represented as a list of 15
pegs. a peg is a 4-tuple of integers: (row, column, peg number, and if
it’s full/empty/invalid). The return type of the recursive functions
are ‘Wins’ which is a tuple of representing the total number of
successes of a board from every subsequent board (if any), including
itself.
