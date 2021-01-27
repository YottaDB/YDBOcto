#!/bin/bash
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

tarball_name=$(ls | grep "yottadb_octo")
# Install YDBPosix files required by Octo
install -D plugin/libydbposix.so $ydb_dist/plugin
install -D plugin/ydbposix.xc $ydb_dist/plugin
install -D plugin/o/_ydbposix.so $ydb_dist/plugin/o
install -D plugin/o/utf8/_ydbposix.so $ydb_dist/plugin/o/utf8

# Reset environment to include new files
temp_dist=$ydb_dist
source $ydb_dist/ydb_env_unset
export ydb_chset=UTF-8
export LC_ALL=en_US.utf8
source $temp_dist/ydb_env_set

# Configure database
rm -f $ydb_dir/$ydb_rel/g/octo.*
export ydb_gbldir="$ydb_dir/$ydb_rel/g/octo.gld"
echo "docker-install.sh: \$ydb_gbldir set to $ydb_gbldir"
echo "docker-install.sh: \$ydb_routines set to $ydb_routines"
$ydb_dist/yottadb -r ^GDE <<FILE
add -segment OCTO -access_method=bg -file_name=$ydb_dir/$ydb_rel/g/octo.dat
add -region OCTO -dynamic=octo -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl") -null_subscripts=always -key_size=1019 -record_size=300000
add -name %ydboctoschema -region=octo
add -name %ydboctoxref -region=octo
add -name %ydboctoocto -region=octo
change -segment DEFAULT -file_name=$ydb_dir/$ydb_rel/g/yottadb.dat
verify
show
exit
FILE
$ydb_dist/mupip create
$ydb_dist/mupip SET -reg DEFAULT -RECORD_SIZE=300000
octo -f northwind.sql
$ydb_dist/mupip load northwind.zwr
# Setup default user
echo -en "ydbrocks\nydbrocks" | $ydb_dist/yottadb -r %ydboctoAdmin add user ydb
