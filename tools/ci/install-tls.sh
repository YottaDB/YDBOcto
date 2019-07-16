#!/bin/bash

source /opt/yottadb/current/ydb_env_set

export gtm_icu_version=`pkg-config --modversion icu-io`

# Install TLS plugin
pushd $ydb_dist/plugin/gtmcrypt
tar -xf source.tar
make
make install
popd
