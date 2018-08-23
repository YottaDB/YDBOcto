#!/bin/bash

source /opt/yottadb/current/ydb_env_set

cd build
make docs
mv html ../public
