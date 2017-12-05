# Chess-haskell

## Overview

A chess rule engine written in Haskell

### Central features:

- encode the full logic of a chess game. This includes, among other features, the following:
  - detect whether a move is a check or taking a piece
  - calculating all possible moves
  - allowing for non-standard moves like EnPassant and castling
- import and export games using PGN
- import and export positions using the FEN notation
- evaluate a position, and a whole game, using Stockfish

### Speculative features
These features are created out of curiousity, and they might change or disappear in later interations of this library

- algorithmically create chess mating puzzles

## Guiding principles

A central goal of this library is that conversions to and from external formats should be safe, with bad inputs being reject. Parsing a PGN should fail if the PGN contains a bad characters or a move that isn't possible. Partially, this is ensured through Haskell's types, but the bulk of this work is done by combining parsing with the chess logic - for instance, a move in a PGN can only be parsed if it corresponds to a legal move that's possible in the latest position. 

To achieve this safety, I am aiming for a very high test coverage on the corresponding logic. One of the key tests is whether the library can correctly parse collections of games, but the unit tests aim to test each piece of the logic separately.

There's natural tradeoff between safety and speed. Consider parsing a PGN into a Haskell `Game`. This parsing could probably be dramatically sped up if one assumes that the PGN describes a correct game. Right now, parsing a game takes up substantial computatation time. In a speed test that I'm running on an AWS instance with a single CPU, parsing 100 games from an external file takes up about 6 seconds. This is fast enough to quickly read individual games, but it means that parsing a large database with millions of games takes a long time. I believe it's possible that a couple of additional improvements can speed up parsing by a factor of 10 or more, which is high on my list of next steps.

## Next steps:

- Speed up game parsing
- Speed up the Stockfish integration
- Parallelize both the game parsing and the Stockfish integration
- Allow for analytics of chess positions. The main goal here is to describe each position through a variety of features. These features can then be used in a statistical analysis of chess games. Examples:
  - what's White's material advantage (combined with the evaluation, allows quantifying the value of the initiative)
  - how many pieces can be taken (this is a measure of how sharp a position is)
  - If a player has the advantage, does realizing this advantage require a number of precise moves or does the player have a wide number of moves that leave her with an advantage?


