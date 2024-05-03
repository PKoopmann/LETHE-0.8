#! /bin/bash

while true
do
    grep "Number of processed clauses: " dlForgetting.log | sed s/[^0-9]*// > data.datX
    rm data.dat
    mv data.datX data.dat
    sleep 1
done

