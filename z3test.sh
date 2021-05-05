#! /bin/bash

for i in $(ls src/out/*.z3)
do
  echo $i
  echo "-----------------"
	z3 $i
	echo "================="
done
