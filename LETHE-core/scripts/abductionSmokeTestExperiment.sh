#! /bin/bash

# for i in `cat ~/Documents/Ontologies/pool_sample/dl/classification/fileorder.txt`; do

for i in `ls -Sr1 ~/Documents/Ontologies/pool_sample/dl/classification/*owl`; do

    echo $i

    i2=/Users/koopmann/Documents/Ontologies/pool_sample/files/${i//[$'\t\r\n']}
    
    ls $i
    
    time gtimeout 300 java -cp target/scala-2.12/lethe-standalone.jar tests.AbductionSmokeTest $i

done

  
