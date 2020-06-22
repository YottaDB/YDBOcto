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

# Set verbose mode so we see each command as it gets executed
set -v
set -x

source /opt/yottadb/current/ydb_env_set

start_dir=$(pwd)
# Below ensures any errors in this script cause it to exit with a non-zero status right away
set -e

# CMake commands are different between CentOS and Ubuntu
# disambiguate them here and make it a varible
if [ -x "$(command -v cmake3)" ]; then
  cmakeCommand="cmake3"
else
  cmakeCommand="cmake"
fi
echo " -> cmakeCommand = $cmakeCommand"

if [ -x "$(command -v ctest3)" ]; then
  ctestCommand="ctest3"
else
  ctestCommand="ctest"
fi
echo " -> ctestCommand = $ctestCommand"

echo "# Install the YottaDB POSIX plugin"
pushd $start_dir
./tools/ci/install_posix.sh $cmakeCommand
popd

echo "# Source the ENV script again to YottaDB environment variables after installing POSIX plugin"
source /opt/yottadb/current/ydb_env_unset
source /opt/yottadb/current/ydb_env_set
echo " -> Done setting up POSIX plugin"
echo " -> ydb_routines: $ydb_routines"

echo "# Download and Install BATS testing framework"
cd $start_dir
mkdir build
cd build
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
cd ..

echo "# Download PostgreSQL JDBC driver for testing"
export JDBC_VERSION=42.2.12
wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar

echo "# Check repo for unused outref files"
pushd ../tests
unused_outrefs=$(../tools/ci/find_unused_outrefs.sh)
if [ "$unused_outrefs" != "" ]; then
  echo " -> Unused outrefs found!"
  echo "$unused_outrefs"
  exit 1
fi
popd

echo "# Check repo for unused test files"
pushd ../cmake
unused_tests=$(../tools/ci/find_unused_tests.sh)
if [ "$unused_tests" != "" ]; then
  echo " -> Unused test files found!"
  echo "$unused_tests"
  exit 1
fi
popd

echo "# Randomly choose to test Debug or Release build"
if [[ $(( $RANDOM % 2)) -eq 0 ]]; then
	build_type="Debug"
else
	build_type="RelWithDebInfo"
fi
echo " -> build_type = $build_type"

echo "# Randomly choose whether to use the full test suite or its limited version (prefer full version 3/4 times)"
if [[ $(( $RANDOM % 4)) -eq 0 ]]; then
	full_test="OFF"
else
	full_test="ON"
fi
echo " -> full_test = $full_test"

echo "# Randomly choose whether to use to test installation or in build directory (prefer installation 3/4 times)"
if [[ $(( $RANDOM % 4)) -eq 0 ]]; then
	disable_install="ON"
else
	disable_install="OFF"
fi
echo " -> disable_install = $disable_install"

echo "# Configure the build system for Octo"
${cmakeCommand} -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin -DCMAKE_BUILD_TYPE=$build_type -DFULL_TEST_SUITE=$full_test -DDISABLE_INSTALL=$disable_install ..
if [[ $? -ne 0 ]]; then
	exit 1
fi

echo "# Compile Octo"
make -j `grep -c ^processor /proc/cpuinfo` 2> make_warnings.txt

echo "# Check for unexpected warnings and error/exit if unexpected errors are found"
../tools/ci/sort_warnings.sh
echo " -> Checking for unexpected warning(s)... "
# We do not want any failures in "diff" command below to exit the script (we want to see the actual diff a few steps later).
# So disable the "set -e" setting temporarily for this step.
set +e
if [ -x "$(command -v yum)" ]; then
	if [[ $build_type == "Debug" ]]; then
		diff ../tools/ci/expected_warnings-centos.ref sorted_warnings.txt &> differences.txt
	else
		diff ../tools/ci/expected_warnings-centos_release.ref sorted_warnings.txt &> differences.txt
	fi
else
	if [[ $build_type == "Debug" ]]; then
		diff ../tools/ci/expected_warnings.ref sorted_warnings.txt &> differences.txt
	else
		diff ../tools/ci/expected_warnings-release.ref sorted_warnings.txt &> differences.txt
	fi
fi

# Re-enable "set -e" now that "diff" is done.
set -e

if [ $(wc -l differences.txt | awk '{print $1}') -gt 0 ]; then
  echo " -> Expected warnings differ from actual warnings! diff output follows"
  echo " -> note: '<' indicates an expected warning, '>' indicates an actual warning"
  cat differences.txt
  exit 1
fi

echo "# make install"
make install

if [ -z $USER ]; then
  echo " -> export USER=root"
  export USER=root
fi

echo "# Start PostgreSQL Server"
if [ -f /etc/init.d/postgresql ]; then
  /etc/init.d/postgresql start
else
  # Blindly assuming we are CentOS
  cp ../tools/ci/postgres-centos/postgresql-setup /usr/bin/postgresql-setup
  chmod +x /usr/bin/postgresql-setup
  postgresql-setup initdb
  mv ../tools/ci/postgres-centos/postgresql.conf /var/lib/pgsql/data/postgresql.conf
  chown -v postgres.postgres /var/lib/pgsql/data/postgresql.conf
  su postgres -c "/usr/bin/postgres -D /var/lib/pgsql/data -p 5432" &
  sleep 2
fi

echo "# Make the current user a superuser"
su - postgres -c psql <<PSQL
create user $USER;
alter user $USER SUPERUSER;
PSQL

echo "# Setup for tests"
pushd src
$ydb_dist/mupip set -n=true -reg '*'

echo "# Source ydb_env_set after building and installing Octo"
if [[ $disable_install == "OFF" ]]; then
	source /opt/yottadb/current/ydb_env_unset
	source /opt/yottadb/current/ydb_env_set
	echo " -> Done setting up Octo plugin"
	echo " -> ydb_routines: $ydb_routines"
else
	export ydb_routines="$(pwd)/_ydbocto.so $ydb_routines"
	echo " -> ydb_routines: $ydb_routines"
fi

echo "# Load the data required for tests"
./octo -f ../../tests/fixtures/names.sql
$ydb_dist/mupip load ../../tests/fixtures/names.zwr
popd

echo "# Run the tests"
# We do not want any failures in "ctest" to exit the script (need to do some cleanup so the artifacts
# are not that huge etc.). So disable the "set -e" setting temporarily for this step.
set +e
${ctestCommand} -j `grep -c ^processor /proc/cpuinfo`
exit_status=$?
echo " -> exit_status from ${ctestCommand} = $exit_status"
# Re-enable "set -e" now that ctest is done.
set -e

if [[ 0 == $exit_status ]]; then
	if [[ $build_type != "RelWithDebInfo" ]]; then
		echo "# Rebuild Octo for packaging as it wasn't a RelWithDebInfo build"
		${cmakeCommand} -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin ..
		make -j `grep -c ^processor /proc/cpuinfo`
	fi

	if [[ $cmakeCommand == "cmake" ]]; then
		echo "# Ubuntu pipelines only: Copy installation files for use in Docker image construction"
		echo " -> Copying installation files for Docker image generation... "
		cp ../tools/ci/install.sh .
		echo "# Create plugin directory structure for later reference by install.sh"
		mkdir -p plugin/r plugin/o/utf8 plugin/octo/
		echo "# Copy YDBPosix into build directory for later access by install.sh"
		cp $ydb_dist/plugin/libydbposix.so plugin
		cp $ydb_dist/plugin/ydbposix.xc plugin
		cp $ydb_dist/plugin/o/_ydbposix.so plugin/o
		cp $ydb_dist/plugin/o/utf8/_ydbposix.so plugin/o/utf8
		echo "# Copy Octo-specific dependencies for later access by install.sh"
		cp ../src/aux/*.m plugin/r
		cp ../tests/fixtures/octo-seed.* plugin/octo
		cp ../tests/fixtures/northwind.* plugin/octo
		cp ../src/aux/octo.conf.default plugin/octo/octo.conf
	fi

	echo "# Build binary package"
	make package
fi

echo "# Cleanup files and directories that don't need to be included in the pipeline artifacts"
rm -rf CMakeFiles
rm -rf _CPack_Packages
rm -rf bats-test.*/go
rm -f postgresql*.jar
rm -f *.cmake
rm -f src/test_*	# these are the unit test case executables (should not be needed otherwise)

echo " -> exit $exit_status"
# Unset verbose mode before printing summary of failure results if any
set +x
set +v

if [[ 0 != $exit_status ]]; then
	echo "# ----------------------------------------------------------"
	echo "# List of failed tests/subtests and their output directories"
	echo "# ----------------------------------------------------------"
	grep -A 6 -E "not ok|Test: " Testing/Temporary/LastTest.log | grep -E "not ok|# Temporary|Test: " | grep -C 1 "not ok" | sed "s/^not/  &/;s/^#/  &/"
	echo "# -----------------------------"
fi

exit $exit_status
