#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# Set verbose mode so we see each command as it gets executed
set -v
set -x
set -u # Enable detection of uninitialized variables.
set -o pipefail # this way $? is set to zero only if ALL commands in a pipeline succeed. Else only last command determines $?
set -e # Below ensures any errors in this script cause it to exit with a non-zero status right away

# Install BATS
pushd /tmp/
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
popd

# The VistA Image is Centos, so use cmake3
echo "Current directory is: $PWD"
echo "# Compiling Octo"
mkdir build && cd build
# VISTA_ENV_FILE contains all the environment variables ($gtm*, $ydb*) needed to run VistA.
cmake3 -D TEST_VISTA=ON -D VISTA_ENV_FILE="~vehu/etc/env" ..
make -j $(getconf _NPROCESSORS_ONLN) install
# ARGS="-V" passes -V to ctest, allowing us to see verbose output.
make test ARGS="-V"
