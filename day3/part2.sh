#!/bin/sh

grep -Eo "mul\([0-9]+,[0-9]+\)|do(n't)?\(\)" <test_input2 |
sed -E 's/mul\(([0-9]+),([0-9]+)\)/\1 \2/' |
awk 'BEGIN{r = 1}
     NF==1{r = $0=="do()"}
     NF==2&&r{ s += $1*$2}
     END{print s}'
