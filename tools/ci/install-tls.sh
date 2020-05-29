#!/bin/bash -v
#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

source /opt/yottadb/current/ydb_env_set

export ydb_icu_version=`pkg-config --modversion icu-io`

# Install TLS plugin
pushd $ydb_dist/plugin/gtmcrypt
tar -xf source.tar
make
make install
popd
