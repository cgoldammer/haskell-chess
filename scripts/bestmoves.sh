#!/bin/bash
position=${1:-fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w - 0 1}
movetime=${2:-500}
number_moves=${3:-5}

expect simple.exp "$position" "$movetime" "$number_moves" > results.txt 2>&1
