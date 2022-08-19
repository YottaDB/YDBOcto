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
# Set verbose mode so we see each command as it gets executed
set -v
set -x
set -u # Enable detection of uninitialized variables.
set -o pipefail # this way $? is set to zero only if ALL commands in a pipeline succeed. Else only last command determines $?
set -e # Below ensures any errors in this script cause it to exit with a non-zero status right away

# Copy of the same function in tools/ci/build.sh
cleanup_before_exit() {
	echo "# Cleanup files and directories that don't need to be included in the pipeline artifacts"
	rm -rf CMakeFiles _CPack_Packages bats-test.*/go src/CMakeFiles || true
	rm -f postgresql*.jar ./*.cmake || true
	rm -f src/test_* || true	# these are the unit test case executables (should not be needed otherwise)
	rm -f src/*.dbg
}
trap cleanup_before_exit EXIT

# Install BATS
pushd /tmp/
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
popd

echo "Current directory is: $PWD"
echo "# Compiling Octo"
mkdir build && cd build
# TEST_VISTA_ENV_FILE contains all the environment variables ($gtm*, $ydb*) needed to run VistA.
cmake -D TEST_VISTA=ON -D TEST_VISTA_ENV_FILE="~vehu/etc/env" ..
make -j $(getconf _NPROCESSORS_ONLN) install

set +e
# ARGS="-V" passes -V to ctest, allowing us to see verbose output.
make test ARGS="-V"
exit_status=$?
echo " -> exit_status from make test = $exit_status"
set -e

echo "Printing a sample of the VistA DDL SQL for a spot check"
tail -100 ./vista.sql

# NB: Copied from tools/ci/build.sh
# Unset verbose mode as the below for loop and bats-test.* usages can print thousands of lines
# and/or very long lines that can pollute the pipeline console output
set +v
set +x
# Find out list of passed bats dirs. sortis not necessary but nice to have.
find . -maxdepth 1 -type d | sed 's#^\./##' | grep '^bats-test' | sort > all_bats_dirs.txt
# Find out list of failed bats dirs. sort is not necessary but nice to have.
# Note that "grep" exits with status of 1 if no match is found. But we do not want the script to error (due to "set -e")
# because of this ("set -o pipefail" will cause final exit status of pipeline to be non-zero if at least one of the
# commands exits with a non-zero status). Hence the "|| true" usage below.
grep "Temporary files in" Testing/Temporary/LastTest.log || true | awk '{print $NF}' | sed 's,.*/,,g' | sort > failed_bats_dirs.txt

# Update "passed_bats_dirs.txt" for use by a later stage (to remove passed directories and reduce pipeline artifact size)

# Note down list of bats test directory names and corresponding subtest name in one file
cat ./*/bats_test.out > all_bats_test.out
ls -lart ./*/bats_test.out > lslart_bats_test.out	# this is to note down time stamp of the bats_test.out files
grep '^ok' Testing/Temporary/LastTest.log > passed_bats_subtests.txt || true
grep '^not ok' Testing/Temporary/LastTest.log > failed_bats_subtests.txt || true
touch summary_bats_dirs.txt passed_bats_dirs.txt

# Find out list of bats dirs corresponding to passed subtests.
for tstdir in bats-test.*
do
	if [[ ! -d $tstdir || ! -e $tstdir/bats_test.out ]]; then
		# Don't mark the database prep directory as suspect
		if [ -f $tstdir/OctoImport.log ]; then
			rm -f $tstdir/octo.dat # 10 MB compressed file!!!!
			continue
		fi

		# $tstdir is not a directory OR it does not contain the file bats_test.out.
		# Cannot determine if this is a failed or passed or timedout bats test.
		echo "SUSPECT : $tstdir" >> summary_bats_dirs.txt
		continue
	fi
	cd $tstdir
	subtest=$(sed 's/.*subtest \[//;s/].*//;' bats_test.out)
	# Need -F below in case there are any special characters in the subtest name (e.g. '*')
	# We do not want to treat those as regex in the grep.
	passed=$(grep -F -c "$subtest" ../passed_bats_subtests.txt) || true
	failed=$(grep -F -c "$subtest" ../failed_bats_subtests.txt) || true
	if [[ $((passed + failed)) -gt 1 ]]; then
		echo " --> Multiple subtests with name [$subtest] found in passed_bats_subtests.txt and/or failed_bats_subtests.txt"
		echo " --> Please first fix ambiguity by giving the subtests unique names. Exiting."
		echo " --> List of subtests found is pasted below."
		grep -F "$subtest" ../passed_bats_subtests.txt ../failed_bats_subtests.txt
		exit 1
	elif [[ $passed -eq 1 ]]; then
		echo "PASSED  : $tstdir : $subtest" >> ../summary_bats_dirs.txt
		echo $tstdir >> ../passed_bats_dirs.txt
	elif [[ $failed -eq 1 ]]; then
		echo "FAILED  : $tstdir : $subtest" >> ../summary_bats_dirs.txt
	else
		# It has to be a timed out test. It is also possible some passed/failed subtests show up here
		# in case "$subtest" matched multiple lines. If so, treat that as a timedout directory for now.
		echo "TIMEDOUT : $tstdir : $subtest" >> ../summary_bats_dirs.txt
	fi
	cd ..
done
if [[ -s passed_bats_dirs.txt ]]; then
	echo '# Remove "bats-test*" directories corresponding to passed subtests (reduces pipeline artifact size)'
	# Unset verbose mode as the below can print a very long line of output
	# and pollute the pipeline console output
	set +v
	set +x
	rm -rf $(cat passed_bats_dirs.txt)
	# Restore verbose output now that for long line of output is done
	set -v
	set -x
fi
exit $exit_status
