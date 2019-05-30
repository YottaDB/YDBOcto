#!/bin/bash

source /opt/yottadb/current/ydb_env_set

# Install TLS plugin
pushd $ydb_dist/plugin/gtmcrypt
tar -xf source.tar
make
popd
