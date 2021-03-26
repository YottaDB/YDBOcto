#!/bin/bash
#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
set -e

# Download, Compile, and Install the YottaDB POSIX plugin
# This
if [[ ! $1 =~ "cmake" ]]; then
	echo "Please specify a CMake command, i.e. 'cmake' or 'cmake3' (CentOS)."
	exit 1
fi
pushd /root
git clone https://gitlab.com/YottaDB/Util/YDBPosix.git
cd YDBPosix
mkdir build
cd build
${1} ..
make
make install
popd
