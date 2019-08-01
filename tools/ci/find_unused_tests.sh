#!/bin/bash

for i in $(ls -p ../tests | grep -v "/$" | sed -E 's/\.bats\.in//'); do
  if [ 0 -eq $(grep -w $i bats-tests.cmake | wc -l) ]
  then
    echo "$i.bats.in"
  fi
done
