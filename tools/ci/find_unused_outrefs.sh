#!/bin/bash

for i in $(ls outref/ | sed 's/\.ref//'); do
  if [ 0 -eq $(grep -rw $i | wc -l) ]
  then
    echo "$i.ref"
  fi
done
