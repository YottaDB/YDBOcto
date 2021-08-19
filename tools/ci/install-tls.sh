#!/bin/bash -v
#################################################################
#								#
# Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
source /opt/yottadb/current/ydb_env_set
set -e

ydb_icu_version=$(pkg-config --modversion icu-io)
export ydb_icu_version

# Install TLS plugin
git clone "https://gitlab.com/YottaDB/Util/YDBEncrypt.git" "$ydb_dist/plugin/gtmcrypt"
pushd "$ydb_dist/plugin/gtmcrypt"
make
make install
popd
