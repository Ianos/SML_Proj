#!/bin/bash
# This is a mass testing script for some random generated
# testcases by Konstantinos Ameranis for the EklogikoSagonas problem
# in pl1 ECE NTUA course

# First arguement: your executable (eg ./agonas)
# Second arguement: a suffix in order to distinguish between output from different programs
for i in {1..7}
do
    $1 <"test${i}.in" >"test${i}_${2}.out"
    diff "test${i].out" "test${i}_${2}.out"
done
