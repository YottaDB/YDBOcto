#!/bin/bash
#################################################################
#								#
# Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	#
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
pushd /root
git clone https://gitlab.com/YottaDB/Util/YDBPosix.git
cd YDBPosix
mkdir build
cd build
cmake ..
make
make install
popd
