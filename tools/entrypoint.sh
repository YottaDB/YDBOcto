#!/bin/bash
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

# Set environment variables
source /opt/yottadb/current/ydb_env_set
export ydb_gbldir="$ydb_dir/$ydb_rel/g/octo.gld"
echo "Using global directory file: $ydb_gbldir"
# Run the rocto service
rocto
