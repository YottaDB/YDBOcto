#!/bin/bash

source /opt/yottadb/current/ydb_env_set

mkdir build
cd build
cmake ..
make && make test
