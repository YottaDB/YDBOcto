#!/bin/bash
#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# This script is what runs in the official Octo Docker Image

# If /data/octo.conf doesn't exist, it means that user passed in their own database
# Therefore, do the set-up that was previously done in the docker file
if [ ! -f /data/octo.conf ]; then
	cp /opt/yottadb/current/plugin/octo/octo.conf /data/octo.conf
	sed -i 's/address = "127.0.0.1"/address = "0.0.0.0"/' /data/octo.conf
	source /opt/yottadb/current/ydb_env_set
	octo -f $ydb_dist/plugin/octo/northwind.sql
	mupip load $ydb_dist/plugin/octo/northwind.zwr
	printf "ydbrocks\nydbrocks" | "$ydb_dist/yottadb" -r %ydboctoAdmin add user ydb
	source /opt/yottadb/current/ydb_env_unset
fi

# Set environment variables
source /opt/yottadb/current/ydb_env_set
# Run the rocto service (Must use exec to that CTRL-C goes to Rocto, not the bash script)
exec rocto -v
