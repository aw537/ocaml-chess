OCaml Chess

The only external package is OUnit2. Everything else is written in vanilla OCaml, so there aren't any special installation instructions.

To run our test cases, run "make test".
To build, run "make build".
To play the game, run "make play".

When playing, there are a few commands
- "quit": Quits the game
- To play a move, type "move fr:f'r' where
- - f is the old file, a character a-h
- - r is the old rank, a number 1-8
- - f' is the new file, a character a-h
- - r' is the new rank, a number 1-8
- "show fr": Shows the possible moves for piece at file f and rank r
