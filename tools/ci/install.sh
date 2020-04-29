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

# An exhaustive list of installation files can be found in src/CMakeLists.txt (those with `install` rules).
# Any modification to those rules will likely need to be reflected here.

# Install all requisite files for proper Octo/Rocto operation
install -d $ydb_dist/plugin/octo
# Install YDBPosix files required by Octo
install -D plugin/libydbposix.so $ydb_dist/plugin
install -D plugin/ydbposix.xc $ydb_dist/plugin
install -D plugin/o/_ydbposix.so $ydb_dist/plugin/o
install -D plugin/o/utf8/_ydbposix.so $ydb_dist/plugin/o/utf8
# Install ydbocto.ci, octo.conf, and octo-seed.* files
install -D src/ydbocto.ci $ydb_dist/plugin/octo/
install -D plugin/octo/octo-seed.* $ydb_dist/plugin/octo/
# Don't overwrite octo.conf if it already exists
if [[ ! -f "$ydb_dist/plugin/octo/octo.conf" ]]; then
	install -D -m 644 plugin/octo/octo.conf $ydb_dist/plugin/octo
fi
# Install binaries and create softlinks to them
install -d $ydb_dist/plugin/octo/bin
install -D src/*octo $ydb_dist/plugin/octo/bin
install -d $ydb_dist/plugin/bin/
ln -s $ydb_dist/plugin/octo/bin/*octo $ydb_dist/plugin/bin/
# Install Octo M routines and object files
install -d $ydb_dist/plugin/r
install -D -m 644 plugin/r/* $ydb_dist/plugin/r
install -D -m 644 src/_ydbocto.so $ydb_dist/plugin/o
install -D -m 644 src/utf8/_ydbocto.so $ydb_dist/plugin/o/utf8
# Reset environment to include new files
temp_dist=$ydb_dist
source $ydb_dist/ydb_env_unset
export ydb_chset=UTF-8
export LC_ALL=en_US.utf8
source $temp_dist/ydb_env_set
# Configure database
rm -f $ydb_dir/$ydb_rel/g/octo.*
export ydb_gbldir="$ydb_dir/$ydb_rel/g/octo.gld"
echo "install.sh: \$ydb_gbldir set to $ydb_gbldir"
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
# Load PostgreSQL seed data
octo -f $ydb_dist/plugin/octo/octo-seed.sql
ydb_chset="" LC_ALL=C $ydb_dist/mupip load $ydb_dist/plugin/octo/octo-seed.zwr
octo -f plugin/octo/northwind.sql
$ydb_dist/mupip load plugin/octo/northwind.zwr
# Setup default user
echo -n ydbrocks | $ydb_dist/yottadb -r %ydboctoAdmin add user ydb
