#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
set -e

# Download, Compile, and Install the YottaDB YDBAIM plugin
pushd /root
git clone https://gitlab.com/YottaDB/Util/YDBAIM.git
cd YDBAIM
# On the pipeline, there is only a single YottaDB, and it should be found by
# pkg-config (called by CMake); thus we don't need to set ydb_dist.
./install.sh
popd
