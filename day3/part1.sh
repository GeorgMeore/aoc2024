#!/bin/sh

grep -Eo 'mul\([0-9]+,[0-9]+\)' <input |
sed -E 's/mul\(([0-9]+),([0-9]+)\)/\1 \2/' |
awk '{s += $1*$2}
     END{print s}'
