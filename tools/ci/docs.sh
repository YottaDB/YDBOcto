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

# Download, Compile, and Install the YottaDB POSIX plugin
# Install the YottaDB POSIX plugin
./tools/ci/install_posix.sh cmake

cd doc

if [ -e Makefile ]; then
	make html
else
	ninja html
fi
mv _build/html ../public
