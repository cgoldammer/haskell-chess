#!/bin/bash
position=$1
# position="fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w - 0 1"
echo "Position: $position"
expect simple.exp "$position" > results.txt 2>&1
# cat results.txt
