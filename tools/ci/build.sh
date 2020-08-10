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

# If we found a recent enough version, run clang-format
if CLANG_FORMAT="$(../tools/ci/find-clang-format.sh)"; then
	echo "# Check code style using clang-format"
	# This modifies the files in place so no need to record the output.
	../tools/ci/clang-format-all.sh $CLANG_FORMAT
# RHEL/CentOS 7 has an outdated version of clang-format, but we run it in pipelines.
# Ignore failures only on this platform.
elif [ -x "$(which rpm)" ] && ! grep 'VERSION_ID=.*7' /etc/os-release; then
	# Otherwise, fail the pipeline.
	echo " -> A recent enough version of clang-format was not found!"
	exit 1
fi

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
export CTEST_OUTPUT_ON_FAILURE=TRUE
${cmakeCommand} -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin -DCMAKE_BUILD_TYPE=$build_type -DFULL_TEST_SUITE=$full_test -DDISABLE_INSTALL=$disable_install ..
if [[ $? -ne 0 ]]; then
	exit 1
fi

echo "# Compile Octo"
# We do not want any failures in "make" to exit the script (need to print the build errors into stdout)
# So disable the "set -e" setting temporarily for this step.
set +e
make -j `grep -c ^processor /proc/cpuinfo` 2> make_warnings.txt
exit_status=$?
if [[ 0 != $exit_status ]]; then
	echo "# make failed with exit status [$exit_status]. make output follows below"
	cat make_warnings.txt
	exit $exit_status
fi
# Re-enable "set -e" now that "make" is done.
set -e

echo "# Check for unexpected warnings and error/exit if unexpected errors are found"
../tools/ci/sort_warnings.sh make_warnings.txt
echo " -> Checking for unexpected warning(s) while compiling ... "
if [ -x "$(command -v yum)" ]; then
	if [[ $build_type == "Debug" ]]; then
		reference=../tools/ci/expected_warnings-centos.ref
	else
		reference=../tools/ci/expected_warnings-centos_release.ref
	fi
else
	if [[ $build_type == "Debug" ]]; then
		reference=../tools/ci/expected_warnings.ref
	else
		reference=../tools/ci/expected_warnings-release.ref
	fi
fi

compare() {
	expected="$1"
	actual="$2"
	# We do not want any failures in "diff" command below to exit the script (we want to see the actual diff a few steps later).
	# So never count this step as failing even if the output does not match.
	diff "$expected" sorted_warnings.txt &> differences.txt || true

	if [ $(wc -l differences.txt | awk '{print $1}') -gt 0 ]; then
		echo " -> Expected warnings differ from actual warnings! diff output follows"
		echo " -> note: '<' indicates an expected warning, '>' indicates an actual warning"
		cat differences.txt
		exit 1
	fi
}
compare $reference

# `clang-tidy` is not available on CentOS 7, and YDB tests on 7 to ensure backwards-compatibility.
if ! [ -x "$(command -v yum)" ]; then
	echo "# Check for unexpected warning(s) from clang-tidy ..."
	../tools/ci/clang-tidy-all.sh > clang_tidy_warnings.txt 2>/dev/null
	../tools/ci/sort_warnings.sh clang_tidy_warnings.txt
	# In release mode, `assert`s are compiled out and clang-tidy will emit false positives.
	if [ "$build_type" = Debug ]; then
		compare ../tools/ci/clang_tidy_warnings.ref
	else
		compare ../tools/ci/clang_tidy_warnings-release.ref
	fi
fi

echo "# prepare binary tarball"
# Declare the tarball generation logic as a function in case we need to rebuild in release mode for Docker image creation
create_tarball() {
	# Gather elements of tarball name format: yottadb_octo_<octo_version>_<os_id><os_version>_<platform_arch>_pro.tar.gz
	octo_version="$(src/octo --version | grep "Octo version" | cut -f 3 -d ' ')"
	os_id="$(../tools/get_platform_name.sh)"
	os_version="$(../tools/get_platform_version.sh)"
	platform_arch="$(../tools/get_platform_arch.sh)"
	if [[ -f $ydb_dist/plugin/libgtmtls.so ]]; then
		tls_support="tls_"
	fi
	tarball_name="yottadb_octo_${octo_version}_${tls_support}${os_id}${os_version}_${platform_arch}_pro"

	# Transfer requisite files into tarball directory and compress
	echo "# Create plugin directory structure for later reference by [octo]install.sh"
	mkdir -p $tarball_name/plugin/r $tarball_name/plugin/o/utf8 $tarball_name/plugin/octo/bin
	echo "# Copy YDBPosix into build directory for later access by [octo]install.sh"
	cp $ydb_dist/plugin/libydbposix.so $tarball_name/plugin
	cp $ydb_dist/plugin/ydbposix.xc $tarball_name/plugin
	cp $ydb_dist/plugin/o/_ydbposix.so $tarball_name/plugin/o
	cp $ydb_dist/plugin/o/utf8/_ydbposix.so $tarball_name/plugin/o/utf8
	echo "# Copy Octo-specific dependencies for later access by [octo]install.sh"
	cp octoinstall.sh $tarball_name
	cp ../tools/get_ydb_release.sh $tarball_name
	cp ../tools/get_platform_name.sh $tarball_name
	cp ../tools/get_platform_version.sh $tarball_name
	cp ../tools/get_platform_arch.sh $tarball_name
	cp ../src/aux/*.m $tarball_name/plugin/r
	cp src/ydbocto.ci $tarball_name/plugin/octo
	cp ../tests/fixtures/octo-seed.* $tarball_name/plugin/octo
	cp ../src/aux/octo.conf.default $tarball_name/plugin/octo/octo.conf
	echo "# Copy Octo binaries and libraries for later access by [octo]install.sh"
	cp src/octo src/rocto $tarball_name/plugin/octo/bin
	cp src/_ydbocto.so $tarball_name/plugin/o
	cp src/utf8/_ydbocto.so $tarball_name/plugin/o/utf8
	echo "# Copy .dbg files for debugging RelWithDebInfo builds"
	if [[ -f src/octo.dbg && -f src/rocto.dbg ]]; then
		cp src/*.dbg $tarball_name/plugin/octo
	fi

	echo "# Build binary package"
	tar -czvf $tarball_name.tar.gz $tarball_name
}
create_tarball

echo "# Randomly choose to install from tarball or via make install"
if [[ $(( $RANDOM % 2)) -eq 0 ]]; then
	echo "# install from tarball"
	cd $tarball_name
	./octoinstall.sh
	cd ..
else
	echo "# make install"
	make install
fi

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

# Force password authentication for PSQL by revising and reloading the config file. This is needed to prevent authentication
# failures of the form "FATAL: Ident authentication failed for user ..." when attempting to connect to the PostgreSQL server.
if [[ $cmakeCommand == "cmake" ]]; then
	# Ubuntu
	psql_conf=$(find /etc/postgresql -name "pg_hba.conf")
else
	# CentOS
	psql_conf=$(find /var/lib/pgsql -name "pg_hba.conf")
fi
sed -i "s/ident/md5/" $psql_conf
psql postgres <<PSQL
SELECT pg_reload_conf();
PSQL

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
	if [[ $build_type != "RelWithDebInfo" || $disable_install != "OFF" ]]; then
		echo "# Rebuild Octo for packaging as it wasn't a RelWithDebInfo build or was built with installation disabled"
		${cmakeCommand} -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin -DCMAKE_BUILD_TYPE=RelWithDebInfo -DDISABLE_INSTALL=OFF ..
		make -j `grep -c ^processor /proc/cpuinfo`
		create_tarball
	fi

	if [[ $cmakeCommand == "cmake" ]]; then
		echo "# Ubuntu pipelines only: Copy installation script into tarball directory for use in Docker image construction"
		cp ../tools/ci/docker-install.sh $tarball_name
		echo "# Copy dummy data for use in Docker image. No other fixtures are needed as Northwind tests full functionality"
		cp ../tests/fixtures/northwind.* $tarball_name
	fi
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
