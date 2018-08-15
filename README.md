# Haskell-chess

## Overview

A chess rule library written in Haskell. This library is meant for real-world use. Right now, it provides the chess logic that's used in the backend (https://github.com/cgoldammer/chess-database-backend) of the chess database I built (https://chessinsights.org).

I have tried to emphasize both correctness (Haskell's types and many unit tests) but also speed. The database I built must be able to parse games at reasonable rates, and to achieve that, I often needed to adapt algorithms to vastly speed them up.

### Features:

- encode the full logic of a chess game. This includes, among other features, the following:
  - detect whether a move is a check or taking a piece
  - calculate all possible moves
  - allow for non-standard moves like EnPassant and castling
- import and export games using PGN
- import and export positions using the FEN notation
- evaluate a position, and a whole game, using Stockfish

### Speculative features
These features are created out of curiousity, and they might change or disappear in later interations of this library

- algorithmically create chess mating puzzles
- extract metrics for a game and a position. For instance, we can obtain how many pieces can be captured in a position, which can be combined with other metrics to create a measure of how tactically complicated the position is.

## Guiding principles

A central goal of this library is that conversions to and from external formats should be safe, with bad inputs being reject. Parsing a PGN should fail if the PGN contains a bad characters or a move that isn't possible. Partially, this is ensured through Haskell's types, but the bulk of this work is done by combining parsing with the chess logic - for instance, a move in a PGN can only be parsed if it corresponds to a legal move that's possible in the latest position. 

To achieve this safety, I am aiming for a very high test coverage on the corresponding logic. One of the key tests is whether the library can correctly parse collections of games, but the unit tests aim to test each piece of the logic separately.

There's natural tradeoff between safety and speed. Consider parsing a PGN into a Haskell `Game`. This parsing could probably be dramatically sped up if one assumes that the PGN describes a correct game. Right now, parsing a game takes up substantial computatation time. In a speed test that I'm running on an AWS instance with a single CPU, parsing 100 games from an external file takes up about 6 seconds. This is fast enough to quickly read individual games, but it means that parsing a large database with millions of games takes a long time. I believe it's possible that a couple of additional improvements can speed up parsing by a factor of 10 or more, which is high on my list of next steps.

At the same time, I also want to be able to parse games quickly. This has required some code changes, especially relating to detecting checks. The resulting algorithms are not pretty, but have resulted in enormous speedups.

## Next steps:

- Parallelize the game parsing and the evaluation using Stockfish
