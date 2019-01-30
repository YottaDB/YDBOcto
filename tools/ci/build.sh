#!/bin/bash

source /opt/yottadb/current/ydb_env_set

mkdir build
cd build
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
cd ..
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
make
source activate
pushd src
mupip set -n=true -reg '*'
popd
make test
