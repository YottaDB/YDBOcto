#!/bin/bash

source /opt/yottadb/current/ydb_env_set

mkdir build
cd build
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
cd ..
# Install TLS plugin
pushd $ydb_dist/plugin/gtmcrypt
tar -xf source.tar
make
popd
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
make
source activate
pushd src
$ydb_dist/mupip set -n=true -reg '*'
# Load the data
./octo -f ../../tests/fixtures/names.sql
$ydb_dist/mupip load ../../tests/fixtures/names.zwr
popd
make test
