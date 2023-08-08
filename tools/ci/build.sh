#!/bin/bash
#################################################################
#								#
# Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	#
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

jobname=$1      # Could be "make-rocky", "make-ubuntu", "make-tls-rocky", "make-tls-rocky" or "test-auto-upgrade"
subtaskname=$2  # Could be "force" or "none" in case jobname is "test-auto-upgrade"
                # Could be "asan" for "make-rocky", "make-ubuntu", "make-tls-rocky", "make-tls-rocky"
autoupgrade_old_commit=$3 # Git hash
autoupgrade_test_to_troubleshoot=$4 # specific CMake test name to troubleshoot

# Determine if we are running on Ubuntu or Rocky Linux (Centos 8 successor)
. /etc/os-release

case "$ID" in
	ubuntu)
		is_ubuntu="true"
		is_rocky8="false"
		;;
	rocky)
		is_ubuntu="false"
		is_rocky8="true"
		;;
	*)
		echo "unsupported distribution"
		exit 1
		;;
esac

source /opt/yottadb/current/ydb_env_set
set -u # Enable detection of uninitialized variables. Do *after* ydb_env_set since this script relies on uninitialized variables.
set -o pipefail	# this way $? is set to zero only if ALL commands in a pipeline succeed. Else only last command determines $?
		# For example, this ensures that IF we run a "ninja | ... | grep ..." command somewhere in the following script,
		# it returns a non-zero exit status even if "ninja" fails (due to a build failure) and not just if "grep" fails.

start_dir=$(pwd)
# Below ensures any errors in this script cause it to exit with a non-zero status right away
set -e

# Compel use of `make` for test-auto-upgrade pipeline job to avoid test failures present with Ninja builds.
# See the note at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/863#note_488254228 for details.
if [[ "test-auto-upgrade" == $jobname && "force" == $subtaskname ]]; then
	USE_MAKE=1
else
	USE_MAKE=0
fi

compile_octo() {
	echo "# Compile Octo"
	# We do not want any failures in "make" or "ninja" to exit the script (need to print the build errors into stdout)
	# So disable the "set -e" setting temporarily for this step.
	set +e
	# Use Ninja by default, but allow overriding it to use Make.
	if [ "$USE_MAKE" = 1 ]; then
		make -j $(grep -c ^processor /proc/cpuinfo) 2> build_warnings.txt
	else
		# Only show warnings in the GitLab UI. Show the full output in `build_warnings.txt`.
		# See https://ninja-build.org/manual.html#_environment_variables for the syntax of NINJA_STATUS.
		# We don't want the `grep -v ninja` to fail if there are no ninja lines in build_warnings.txt (normal case
		# when there are no warnings). Hence the `|| true` and a subshell usage.
		NINJA_STATUS="[ninja] [%f/%t] " ninja | tee build_warnings.txt | (grep -v '^\[ninja\] ' || true)
	fi
	exit_status=$?
	if [[ 0 != $exit_status ]]; then
		echo "# $build_tool failed with exit status [$exit_status]. output follows below"
		cat build_warnings.txt
		exit $exit_status
	fi
	# Re-enable "set -e" now that "ninja" is done.
	set -e
}

# This function is invoked before running .sql files using the new octo (from "newsrc").
setup_before_running_sql_files() {
	echo "# Running *.sql files in $tstdir : [subtest : $subtest]" | tee -a ../errors.log
	echo "INCLUDE : $tstdir" >> ../include_bats_test.txt
	# Check if subtest ran in M or UTF-8 mode and switch ydb_chset and ydb_routines accordingly.
	# Search "dbg_env.out" file for whether the chset was M or UTF-8.
	is_utf8=$(grep -c "^ydb_chset=UTF-8" dbg_env.out) || true
	if [[ $is_utf8 == 0 ]]; then
		export ydb_chset=M
		utf8_path="."
	else
		export ydb_chset=UTF-8
		utf8_path="utf8"
	fi
	export ydb_routines=". ../newsrc/$utf8_path/_ydbocto.so $ydb_dist/plugin/o/$utf8_path/_ydbposix.so $ydb_dist/plugin/o/$utf8_path/_ydbaim.so $ydb_dist/$utf8_path/libyottadbutil.so"
	# Create the AIM database if the previous commit predates AIM
	if [ 0 -eq $is_post_ydbaim_commit ]; then
		echo "# Previous commit predates AIM; create AIM database for upgrade"
		$ydb_dist/yottadb -run ^GDE > gde_create_aim.txt 2>&1 << AIM
add -segment AIMSEG -file="$aimdat" -access_method=MM -block_size=2048
add -region AIMREG -dyn=AIMSEG -nojournal -key_size=1019 -null_subscripts=always -record_size=2048
add -name %ydbAIM* -region=AIMREG
exit
AIM
		mupip create -region=AIMREG &>> gde_create_aim.txt
	fi
	# Change absolute path names of database files to relative path names for ease of later debugging (if needed)
	$ydb_dist/yottadb -run GDE >> gde_change_segment.txt 2>&1 << FILE
	change -segment DEFAULT -file_name=$defaultdat
	change -segment OCTOSEG -file_name=$octodat
	change -segment AIMSEG -file_name=$aimdat
FILE
}

if [ "$USE_MAKE" = 1 ]; then
	generator="Unix Makefiles"
	build_tool="make -j $(grep -c ^processor /proc/cpuinfo)"
else
	generator=Ninja
	build_tool=ninja
fi

# Install Posix and AIM plug-ins 50% of the time
# The other 50%, CMake will take care of installing them
# For Auto-upgrade tests, The CMake installer may not exist; therefore, always install the plugins manually.
if [[ $(( RANDOM % 2)) -eq 0 || ("test-auto-upgrade" == $jobname) ]]; then
	echo "# Install the YottaDB POSIX and AIM plugins"
	pushd /tmp/
	wget https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
	chmod +x ydbinstall.sh
	./ydbinstall.sh --plugins-only --aim --posix
	popd
fi

echo "# Source the ENV script again to YottaDB environment variables after installing plugins"
set +u # Temporarily disable detection of uninitialized variables since ydb_env_set relies on them.
source /opt/yottadb/current/ydb_env_unset
source /opt/yottadb/current/ydb_env_set
set -u # Re-enable detection of uninitialized variables
echo " -> Done setting up plugins"
echo " -> ydb_routines: $ydb_routines"

echo "# Download and Install BATS testing framework"
cd $start_dir
mkdir build
cd build
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
cd ..

# Note: The below set of lines also exist in `tests/test_helpers.bash.in` so any change here might need to be made there too.
# Log env vars, shell vars, locale info, ulimit, memory/disk/cpu limits in files for later analysis of test failures.
# Mask any sensitive env vars out.
env | grep -vE "HUB_USERNAME|HUB_PASSWORD|CI_JOB_TOKEN|CI_REGISTRY_PASSWORD|CI_BUILD_TOKEN|CI_REPOSITORY_URL" > dbg_env.out
set | grep -vE "HUB_USERNAME|HUB_PASSWORD|CI_JOB_TOKEN|CI_REGISTRY_PASSWORD|CI_BUILD_TOKEN|CI_REPOSITORY_URL" > dbg_set.out
locale > dbg_locale.out
locale -a > dbg_localeall.out

# Log ulimit, memory/disk/cpu limits in files for later analysis of test failures.
# Note that the below are the same across all subtests so we don't record this in each subtest output directory so
# the below lines are not duplicated in `tests/test_helpers.bash.in`.
ulimit -a > dbg_ulimit.out	# ulimit settings of memory/file descriptors/stack etc.
free > dbg_free.out		# system RAM and SWAP
df -h > dbg_dfh.out		# system disk space
lscpu > dbg_lscpu.out		# system CPUs

# The Postgres driver is now part of the CMake files as of MR !1390 ([#915] Pre-compile Java and other refactoring)
# But previous commits don't have that, so need to get it for previous commits
if [[ "test-auto-upgrade" == $jobname ]]; then
	echo "# Download PostgreSQL JDBC driver for testing"
	export JDBC_VERSION=42.6.0	# this is the latest driver as of May 2023
	wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar
fi

echo "# Check repo for unused outref files"
pushd ../tests
unused_outrefs=$(../tools/ci/find_unused_outrefs.sh)
if [ "$unused_outrefs" != "" ]; then
  echo " -> Unused outrefs found!"
  echo "$unused_outrefs"
  exit 1
fi
popd

# If this is the "test-auto-upgrade" job, skip steps that are covered by other jobs (e.g. "make-ubuntu" etc.)
if [[ "test-auto-upgrade" != $jobname ]]; then
	echo "# Check repo for unused test files"
	pushd ../cmake
	unused_tests=$(../tools/ci/find_unused_tests.sh)
	if [ "$unused_tests" != "" ]; then
	  echo " -> Unused test files found!"
	  echo "$unused_tests"
	  exit 1
	fi
	popd

	# Run clang-format check on the Ubuntu pipeline jobs (i.e. "make-ubuntu" and "make-tls-ubuntu").
	# Do not run it on the Rocky linux pipeline jobs (i.e. "make-rocky" and "make-tls-rocky") as
	# we have seen that Rocky Linux could have different clang-format versions than Ubuntu
	# (at the time of this writing Rocky Linux had clang-format-12 whereas Ubuntu had clang-format-10)
	# and each could format the same C program differently causing pipeline failures. So run the
	# clang-format only on one OS. We pick Ubuntu for now. Hence the "$is_ubuntu" check below.
	if $is_ubuntu; then
		# If we found a recent enough version, run clang-format
		if CLANG_FORMAT="$(../tools/ci/find-llvm-tool.sh clang-format 11)"; then
			echo "# Check code style using clang-format"
			# This modifies the files in place so no need to record the output.
			../tools/ci/clang-format-all.sh $CLANG_FORMAT
		else
			# Otherwise, fail the pipeline.
			echo " -> A recent enough version of clang-format was not found!"
			exit 1
		fi
	fi

	if [ -x "$(command -v shellcheck)" ]; then
		find .. -name build -prune -o -name '*.sh' -print0 | xargs -0 shellcheck -e SC1091,SC2154,SC1090,SC2086,SC2053,SC2046
	else
		echo " -> Shellcheck not found!"
		exit 1
	fi
fi

pushd ..

# Confirm all error message mnemonics and text are included in the documentation
./tools/ci/doc_error_update.sh "check"

# Confirm code base assertions are still valid
./tools/ci/check_code_base_assertions.sh

popd

echo "# Randomly choose to test Debug or Release build"
if [[ $(( RANDOM % 2)) -eq 0 ]]; then
	build_type="Debug"
else
	build_type="RelWithDebInfo"
fi
echo " -> build_type = $build_type"

if [[ "test-auto-upgrade" != $jobname ]]; then
	set +u # Disable Bash's "Treat unset variables as an error" to allow us to try to see if $CI_COMMIT_BRANCH is defined
	# If this is run outside of a pipeline, or if run on the master branch (post-merge)
	if [ -z $CI_COMMIT_BRANCH ] || [ "master" == "$CI_COMMIT_BRANCH" ]; then
		echo "# Randomly choose whether to use the full test suite or its limited version (prefer full version 3/4 times)"
		if [[ $(( RANDOM % 4)) -eq 0 ]]; then
			full_test="OFF"
		else
			full_test="ON"
		fi
	else # We are running on a branch pipeline. Run the full test suite.
		echo "# On branch pipeline, running full test suite"
		full_test="ON"
	fi
	set -u # Enable back
	echo "# Randomly choose whether to test from installed directory OR from build directory (prefer install 3/4 times)"
	if [[ $(( RANDOM % 4)) -eq 0 ]]; then
		disable_install="ON"
	else
		disable_install="OFF"
	fi

	if [ "asan" = $subtaskname ]; then
	  # Enable ASAN. It's only a 2x slowdown
	  # Run full test suite
	  asan="ON"
	  full_test="ON"
	else
	  asan="OFF"
	fi
else
	# Always run the full test suite in case of "test-auto-upgrade" job.
	# That will give us maximum coverage for auto-upgrade testing.
	full_test="ON"
	# Disable installs for "test-auto-upgrade" as we need to run tests with 2 Octo builds and so need to keep those
	# two builds in separate subdirectories and cannot install both into $ydb_dist.
	disable_install="ON"
	# Don't run any ASAN on upgrade jobs.
	asan="OFF"
fi
echo " -> full_test = $full_test"
echo " -> disable_install = $disable_install"
echo " -> enable_asan = $asan"

if [[ ("test-auto-upgrade" == $jobname) && ("force" != $subtaskname) ]]; then
	if [[ "debug" == $subtaskname ]]; then
		cp ../tools/ci/testAutoUpgrade.m .
		if [ -n "$autoupgrade_test_to_troubleshoot" ]; then
			# ALL is a special value that we use to troublshoot all tests
			if [[ "ALL" != $autoupgrade_test_to_troubleshoot ]]; then
				echo " ;;$autoupgrade_test_to_troubleshoot" >> testAutoUpgrade.m
			fi
		fi
		commitsha=$autoupgrade_old_commit
	else
		if [[ $CI_COMMIT_BRANCH == "" ]]; then
			# This is possible if the pipeline runs for example when a new tag is created on a pre-existing commit.
			# (for example when the r1.0.0 tag was created). In this case, treat this job as a success.
			echo "INFO : CI_COMMIT_BRANCH env var is empty"
			echo "INFO : Cannot run the test-auto-upgrade test in this case. Exiting with success."
			exit 0
		fi
		# Record git log --all output in a file just in case it helps later. Not used by this script.
		git log --graph --all --oneline --pretty=format:'%h%d; %ai; %an; %s' > gitlogall.txt
		# Checkout a random prior commit to test if auto-upgrade of plans/xrefs/triggers/binary-table-definitions etc.
		# from that commit to the current/latest commit works fine in Octo.
		git checkout -B $CI_COMMIT_BRANCH HEAD
		# Copy M program that is needed for later before we switch to an older git branch.
		cp ../tools/ci/testAutoUpgrade.m .
		# Do not go prior to the hard stop commit (SHA pasted below) as python as python2 is not available anymore on Ubuntu 22.04.
		# This commit remediated the use of Python2 and changed it to Python3
		# Python2 is in tests/fixtures/sqllogic/insert.py code.
		hardstopcommit=eefa4a6f66a05a5cfab97546738231f4952b54b4
		#############################################################################################
		# First verify requirements for this test to succeed. If any of those are not met, just return success right away
		# as the auto upgrade test is not possible.
		#############################################################################################
		# Find common ancestor of upstream/master and HEAD. That is where we stop the search for a random commit
		upstream_URL=https://gitlab.com/YottaDB/DBMS/YDBOcto
		if ! git remote | grep -q upstream_repo; then
			git remote add upstream_repo "$upstream_URL"
			git fetch upstream_repo
		fi
		stopcommit=$(git merge-base HEAD upstream_repo/master)
		# Find HEAD commit to verify its not the same as $stopcommit
		HEAD_COMMIT_ID=$(git rev-list HEAD~1..HEAD)
		if [[ "$stopcommit" == "$HEAD_COMMIT_ID" ]]; then
			echo "INFO : HEAD commit and stopcommit is the same. No in between commits to choose from."
			echo "INFO : Back off stopcommit by 1 commit"
			stopcommit=$(git rev-list HEAD~2..HEAD~1)
		fi
		# Find common ancestor of $hardstopcommit and $stopcommit. Verify it is $hardstopcommit. If not, we cannot test.
		startcommit=$(git merge-base $hardstopcommit $stopcommit)
		if [[ "$startcommit" != "$hardstopcommit" ]]; then
			echo "INFO : Ancestor commit of $hardstopcommit and $stopcommit was not the former but instead is [$startcommit]"
			echo "INFO : Cannot run the test-auto-upgrade test in this case. Exiting with success."
			exit 0
		fi
		#############################################################################################
		# Now that we verified that requirements for this test are met, go ahead with the actual test.
		#############################################################################################
		git log --graph --oneline $startcommit~1..$stopcommit > gitlogmaster.txt
		# Note: The awk usage below is needed to only skip commits that branch off an otherwise linear commit history.
		awk '($1 == "*") && ($2 != "|") {print $0;}' gitlogmaster.txt > commit_history.txt
		numcommits=$(wc -l commit_history.txt | awk '{print $1}')
		commitnumber=$(shuf -i 1-$numcommits -n 1)
		commitsha=$(head -$commitnumber commit_history.txt | tail -1 | awk '{print $2}')
	fi
	# -----------------------------------------------------------------------------------------------------------
	# Check for older commits that have known issues. If they are chosen, return from this script with success.
	# Note that if "$subtaskname" is not "debug" (i.e. "else" block above), we could have tried a different random
	# older commit in that case until we find a random older commit that does not have any known issues. But if
	# "$subtaskname" is "debug" (i.e. "if" block above), we are presented with a specific older commit. We are not
	# allowed to choose any random older commit. So in that case, we have to return right away. Therefore, we keep
	# this check logic common to the "if" and "else" code paths above and return right away if the randomly chosen
	# or specified older commit has known issues.
	# -----------------------------------------------------------------------------------------------------------
	is_old_commit_with_issues=0
	if [[ "3d03de63" == "$commitsha" ]]; then
		# This commit has a known issue in `tests/fixtures/TOJ03.m` that can cause the TJC001 subtest to time out
		# (see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1035/pipelines for failure details).
		# This is fixed in the immediately next commit (e748ab3f) so do not choose this particular commit
		# as otherwise the "test-auto-upgrade" pipeline job (the current job) will also timeout (see
		# https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/977#note_923383570 for description of failure).
		echo "# Skipping $commitsha as it has a known issue that can cause job to timeout"
		echo "# See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/977#note_923383570 for details"
		is_old_commit_with_issues=1
	fi
	# Check if chosen older commit is in the range c1fab585..fe17e8a2. These commits had an issue that was fixed in
	# 4e25d39d and that could cause various tests to take hours to complete causing the test to timeout in the pipeline
	# or to take more than a day when run in-house (using "debug" as the value of "$subtaskname"). Therefore skip these commits.
	if [[ ("c1fab585" == "$commitsha") || ("6b24314a" == "$commitsha")
			|| ("03736ba5" == "$commitsha") || ("fe17e8a2" == "$commitsha") ]]; then
		echo "# Skipping $commitsha as it has a known issue that can cause job to timeout"
		echo "# See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1375#background for details"
		is_old_commit_with_issues=1
	fi
	# Check if chosen older commit is in the range 7fa4406a..f13aff88 (both commits inclusive)
	# This is because the function text definition is not stored correctly in 7fa4406a (fixed YDBOcto#519) and will be fixed in
	# the commit after f13aff88a. See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1182#note_1301167074 for details.
	start_octo519_commit="bee2b104e52874f79d728c98047a1cad2829e25f"	# 1 commit BEFORE YDBOcto#519 commit
	end_octo519_commit="f13aff88fef422a1b507720323b8a4d96e54da64"	# 1 commit before when YDBOcto#519 issue is fixed
	# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
	set +e
	git merge-base --is-ancestor $commitsha $start_octo519_commit
	is_post_start_octo519_commit=$?
	git merge-base --is-ancestor $commitsha $end_octo519_commit
	is_post_end_octo519_commit=$?
	# Re-enable "set -e" now that "git merge-base" invocation is done.
	set -e
	if [[ (0 != $is_post_start_octo519_commit) && (0 == $is_post_end_octo519_commit) ]]; then
		echo "# Skipping $commitsha as it has a known issue with not storing function text definitions correctly"
		echo "# See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1182#note_1301167074 for details"
		is_old_commit_with_issues=1
	fi
	if [[ 0 != $is_old_commit_with_issues ]]; then
		echo "# Cannot continue with job. Returning with success"
		exit 0
	fi
	# -----------------------------------------------------------------------------------------------------------
	# Now that we know the older commit does not have any issues, proceed with auto upgrade test.
	# -----------------------------------------------------------------------------------------------------------
	echo $commitsha > commit_picked.txt
	echo "# Random older commit picked = $commitsha"
	echo "# Checkout the older commit"
	git checkout $commitsha
	# Due to https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/712 and # https://gitlab.com/YottaDB/DB/YDB/-/issues/661, ensure
	# that test framework files corresponding to an older commit are updated minimally enough so they will work with a
	# later/newer version of ydb_env_set.
	git checkout 8587b12086666c88ea2c8a19b55a736629269907 -- ../tools/get_ydb_release.sh
	sed -i 's/unset ydb_chset/export ydb_chset=M/' ../tests/test_helpers.bash.in
	# Add line to verify_output helper function to remove `..` from test output when Octo is built with Ninja
	# See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/861 for more details.
	sed -i "s,# Filter rule #s and line #s printed by flex in TRACE verbosity level output,sed -i 's/\\\.\\\.PATH/PATH/' clean_output\.txt," ../tests/test_helpers.bash.in
	# If the chosen older commit has "*.sql" or "*.bats.in" files with CREATE TABLE commands containing the GLOBAL
	# keyword with "keys(..)" expressions using a lower case column name, those will no longer work with the master
	# after 7fa4406a (YDBOcto#519) as we expect "keys(..)" to use upper case column names for case insensitive column
	# names and case sensitive column names otherwise. The only tests that use case sensitive column names should be
	# those that were newly introduced in 7fa4406a and they are TC058, TCO59 and TCO60. So excepting those *.sql files,
	# all other "*.sql" or "*.bats.in" files are better updated to ensure upper case column names are used inside "keys(..)"
	# expressions. The following logic takes care of that.
	shopt -s extglob	# enable extended pattern matching feature (we use ! syntax below)
	fixtures="$(ls ../tests/*.bats.in ../tests/fixtures/!(TC058*|TC059*|TC060*).sql)"
	sed -i 's/keys(\(""[A-Za-z_0-9]*""\))/keys(\U\1\E)/g' $fixtures
	# In similar fashion, we could have M programs that generates DDLs (i.e. CREATE TABLE commands) containing "keys(...)"
	# expressions with lower cased column names. Fix those as well.
	fixtures="$(ls ../tests/fixtures/*.m)"
	sed -i 's/keys(\(""""[A-Za-z_0-9]*""""\))/keys(\U\1\E)/g' $fixtures
	shopt -u extglob	# reset now that extended pattern matching feature need is done
	if [ "ALL" != "$autoupgrade_test_to_troubleshoot" ]; then
		# Run only a random fraction of the bats tests as we will be running an auto upgrade test on the same queries
		# once more a little later.
		cp ../cmake/bats-tests.cmake bats-tests.cmake.orig
		# Temporarily switch ydb_routines for running M program (testAutoUpgrade.m)
		saveydbroutines="$ydb_routines"
		export ydb_routines="."	# so testAutoUpgrade.o gets created in current directory
		# cat here to confirm contents
		cat testAutoUpgrade.m
		$ydb_dist/yottadb -run batsTestsChooseRandom^testAutoUpgrade < bats-tests.cmake.orig > bats-tests.cmake.new
		# cat here to confirm contents
		cat bats-tests.cmake.new
		export ydb_routines="$saveydbroutines"	# Switch back to original ydb_routines
		cp bats-tests.cmake.new ../cmake/bats-tests.cmake
	fi
else
	# If a "test-auto-upgrade" job and an old commit was chosen, we could see rare failures.
	# Don't want that to pollute the output.
	# Hence the below CTEST_OUTPUT_ON_FAILURE=TRUE setting is done only in other jobs.
	export CTEST_OUTPUT_ON_FAILURE=TRUE
fi

# This function is duplicated in tools/ci/vistatest.sh
cleanup_before_exit() {
	echo "# Cleanup files and directories that don't need to be included in the pipeline artifacts"
	rm -rf CMakeFiles _CPack_Packages bats-test.*/go src/CMakeFiles || true
	rm -f postgresql*.jar ./*.cmake || true
	rm -f src/test_* || true	# these are the unit test case executables (should not be needed otherwise)
	rm -f src/*.dbg
	if [ -d /octooutput ]; then
		rm -rf /octooutput/*
		cp -r -- * /octooutput
	fi
}
trap cleanup_before_exit EXIT

echo "# Configure the build system for Octo"

randomize_OCTO_INIT_BUFFER_LEN() {
	new_buffer_size=$(( 2 ** (RANDOM % 11) ))
	echo "# Random value of OCTO_INIT_BUFFER_LEN = $new_buffer_size" > OCTO_INIT_BUFFER_LEN.txt
	sed -i "s/OCTO_INIT_BUFFER_LEN [0-9]*/OCTO_INIT_BUFFER_LEN $new_buffer_size/" ../src/octo.h
}

cmake -G "$generator" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin -DCMAKE_BUILD_TYPE=$build_type -DFULL_TEST_SUITE=$full_test -DDISABLE_INSTALL=$disable_install -DENABLE_ASAN=$asan ..
if [[ ("ON" == $asan) ]]; then
	# This is an ASAN job. In this case, we want to test for memory issues so randomly select a power of two to
	# use for altering the size of OCTO_INIT_BUFFER_LEN to test for regressions
	randomize_OCTO_INIT_BUFFER_LEN
fi
compile_octo

# If this is the "test-auto-upgrade" job, skip steps that are covered by other jobs (e.g. "make-ubuntu" etc.)
if [[ "test-auto-upgrade" != $jobname ]]; then
	echo "# Check for unexpected warnings and error/exit if unexpected errors are found"
	../tools/ci/sort_warnings.sh build_warnings.txt sorted_build_warnings.txt
	echo " -> Checking for unexpected warning(s) while compiling ... "

	compare() {
		expected="$1"
		actual="$2"
		full_warnings="$3"
		# We do not want any failures in "diff" command below to exit the script (we want to see the actual diff a few steps later).
		# So never count this step as failing even if the output does not match.
		diff "$expected" "$actual" &> differences.txt || true

		if [ $(wc -l differences.txt | awk '{print $1}') -gt 0 ]; then
			set +x  # don't print these diagnostics twice
			echo " -> Expected warnings differ from actual warnings! diff output follows"
			echo " -> note: '<' indicates an expected warning, '>' indicates an actual warning"
			echo " -> help: you can see the full warnings at $CI_JOB_URL/artifacts/raw/build/$3"
			if echo "$full_warnings" | grep -q clang_tidy; then
				echo " -> help: to generate clang-tidy warnings locally, run 'cmake -D CMAKE_EXPORT_COMPILE_COMMANDS=ON .. && ../tools/ci/clang-tidy-all.sh'"
			fi
			cat differences.txt
			set -x
			exit 1
		fi
	}
	compare /dev/null sorted_build_warnings.txt build_warnings.txt

	echo "# Check for unexpected warning(s) from clang-tidy ..."
	../tools/ci/clang-tidy-all.sh > clang_tidy_warnings.txt 2>/dev/null
	../tools/ci/sort_warnings.sh clang_tidy_warnings.txt sorted_clang_warnings.txt
	# In release mode, `assert`s are compiled out and clang-tidy will emit false positives.
	if [ "$build_type" = Debug ]; then
		compare ../tools/ci/clang_tidy_warnings.ref sorted_clang_warnings.txt clang_tidy_warnings.txt
	else
		compare ../tools/ci/clang_tidy_warnings-release.ref sorted_clang_warnings.txt clang_tidy_warnings.txt
	fi

	$build_tool install
fi

# Skip Postgres setup for the forced auto upgrade job as it does not use psql. All the other jobs use it.
if [[ ("test-auto-upgrade" != $jobname) || ("force" != $subtaskname) ]]; then
	set +u # Temporarily disable detection of uninitialized variables for the following -z null check
	if [ -z $USER ]; then
	  echo " -> export USER=root"
	  export USER=root
	fi
	set -u

	echo "# Start PostgreSQL Server"
	if $is_ubuntu; then
	  /etc/init.d/postgresql start
	elif $is_rocky8; then
	  su postgres -c 'pg_ctl -D /var/lib/pgsql/data initdb'
	  su postgres -c 'pg_ctl -D /var/lib/pgsql/data start'
	fi

	echo "# Make the current user a superuser"
	su - postgres -c psql <<PSQL
	create user $USER;
	alter user $USER SUPERUSER;
PSQL
fi

echo "# Setup for tests"
pushd src
# The below step is not needed by the bats tests as they do their own database initialization (in "init_test" function)
# but cmocka tests do not have any such facility hence require this setup.
$ydb_dist/mupip set -null_subscripts=always -reg '*'

echo "# Source ydb_env_set after building and installing Octo"
if [[ $disable_install == "OFF" ]]; then
	set +u # Temporarily disable detection of uninitialized variables since ydb_env_set relies on them.
	source /opt/yottadb/current/ydb_env_unset
	source /opt/yottadb/current/ydb_env_set
	set -u # Re-enable detection of uninitialized variables
	echo " -> Done setting up Octo plugin"
	echo " -> ydb_routines: $ydb_routines"
else
	# In case DISABLE_INSTALL = ON, we need to set the correct UTF-8 directory for $ZROUTINES
	# In case DISABLE_INSTALL = OFF, ydb_env_set takes care of that for us.
	if [ "$ydb_chset" = "UTF-8" ]; then
		ydb_routines="$(pwd)/utf8/_ydbocto.so $ydb_routines"
	else
		ydb_routines="$(pwd)/_ydbocto.so $ydb_routines"
	fi
	export ydb_routines
	echo " -> ydb_routines: $ydb_routines"
fi

popd

if [[ ("test-auto-upgrade" != $jobname) || ("force" != $subtaskname) ]]; then
	# Force password authentication for PSQL by revising and reloading the config file. This is needed to prevent authentication
	# failures of the form "FATAL: Ident authentication failed for user ..." when attempting to connect to the PostgreSQL server.
	if $is_ubuntu; then
		# Ubuntu
		psql_conf=$(find /etc/postgresql -name "pg_hba.conf")
	elif $is_rocky8; then
		# Rocky Linux
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
	# Parallelize tests to however many CPUs there are on a machine
	CTEST_PARALLEL_LEVEL=$(getconf _NPROCESSORS_ONLN)
	export CTEST_PARALLEL_LEVEL
	if [[ ("test-auto-upgrade" != $jobname) ]]; then
		ctest
	else
		# Ensure that `hello*` tests, e.g. `hello_psql.bats` and `hello_db.bats`, run before tests that depend on them in
		# order to prevent failures when parallelizing test execution in test-auto-upgrade jobs, which use older commits.
		ctest -R "hello"
		ctest -R "test"
	fi
	exit_status=$?
	echo " -> exit_status from ctest = $exit_status"

	# This block and much under it is duplicated in tools/ci/vistatest.sh
	# Re-enable "set -e" now that ctest is done.
	set -e
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
	(grep "Temporary files in" Testing/Temporary/LastTest.log || true) | awk '{print $NF}' | sed 's,.*/,,g' | sort > failed_bats_dirs.txt
	# Update "passed_bats_dirs.txt" for use by a later stage (to remove passed directories and reduce pipeline artifact size)
	# No need to do this for "test-auto-upgrade" job as it does not use this file. And it is actually not correct to run
	# this code for that job as we are running an older commit and it could have multiple subtests with name ambiguity
	# which would then cause this script (that belongs to the latest commit) to incorrectly (and prematurely) "exit 1" below.
	if [[ ("test-auto-upgrade" != $jobname) ]]; then
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
				# $tstdir is not a directory OR it does not contain the file bats_test.out.
				# Cannot determine if this is a failed or passed or timedout bats test.
				echo "SUSPECT : $tstdir" >> summary_bats_dirs.txt
				continue
			fi
			cd $tstdir
			subtest=$(sed 's/.*subtest \[//;s/].*//;' bats_test.out)
			if [ -z "$subtest" ]; then
				# See similar code in "tools/ci/vistatest.sh" for why this check is present.
				# We don't know of any such need here but it might happen in the future so better
				# to avoid such a situation and so have this safety check as a precaution.

				# $tstdir is a directory that does not contain a valid subtest name in bats_test.out.
				# Cannot determine if this is a failed or passed or timedout bats test.
				echo "SUSPECT : $tstdir" >> summary_bats_dirs.txt
				continue
			fi
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
	fi
	# Restore verbose output now that for loop and bats-test.* usages (long/lots-of lines) are done
	set -v
	set -x
fi

if [[ "test-auto-upgrade" != $jobname ]]; then
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
else
	# If this is the "test-auto-upgrade" job, ignore errors in ctest (possible some tests fail because we are running
	# tests using an older Octo commit that had a bug which was fixed afterwards). We will test auto-upgrade on the
	# passing subtests only.
	exit_status=0
	echo "# Cleanup unit test case executables from oldsrc directory"
	cleanup_before_exit
	echo '# Move old commit build of Octo to [oldsrc] directory'
	mv src oldsrc
	echo '# Delete cmake/make artifacts of older Octo build to make way for newer Octo build'
	rm -rf CMakeCache.txt CMakeFiles
	if [[ "debug" == $subtaskname ]]; then
		echo '# Reset git repo to before old commit'
		git reset --hard HEAD
		git checkout -
		echo '# Rebuild Octo using the latest commit branch for the auto-upgrade test'
		cmakeflags="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin"
	elif [[ "force" == $subtaskname ]]; then
		cmakeflags="-DFORCE_BINARY_DEFINITION_AUTO_UPGRADE=ON"	# Force auto upgrade
	else
		echo '# Reset git repo to latest commit branch'
		git reset --hard $CI_COMMIT_BRANCH
		echo '# Rebuild Octo using the latest commit branch for the auto-upgrade test'
		cmakeflags="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=${ydb_dist}/plugin"
	fi
	cmakeflags="$cmakeflags -DCMAKE_BUILD_TYPE=$build_type -DFULL_TEST_SUITE=$full_test"
	cmakeflags="$cmakeflags -DDISABLE_INSTALL=$disable_install"
	# Randomly select a power of two to use for altering the size of OCTO_INIT_BUFFER_LEN to test for regressions
	randomize_OCTO_INIT_BUFFER_LEN

	cmake -G "$generator" $cmakeflags ..
	compile_octo
	echo "# Cleanup unit test case executables from newsrc directory"
	rm -rf src/CMakeFiles
	rm -f src/test_*	# these are the unit test case executables (should not be needed otherwise)
	echo '# Move new commit build of Octo to [newsrc] directory'
	mv src newsrc
	echo '# Delete cmake/make artifacts of newer Octo build'
	rm -rf CMakeCache.txt CMakeFiles
	# Unset verbose mode as the below for loop can print thousands of lines and pollute the pipeline console output
	set +x
	set +v
	if [[ "force" != $subtaskname ]]; then
		echo '# Remove "bats-test*" directories corresponding to failed subtests (if any)'
		rm -rf $(cat failed_bats_dirs.txt)
		echo '# Do auto-upgrade tests on the leftover "bats-test*" directories.'
		gldfile="yottadb.gld"
		export ydb_gbldir=$gldfile
		defaultdat="mumps.dat"
		octodat="octo.dat"
		aimdat="aim.dat"
		touch skip_bats_test.txt gde_change_segment.txt
		# Below set of "ydb_icu_version" env var set is needed for UTF-8 chset in "for" loop further down below.
		# Disable the "set -e" setting temporarily as the "readlink" invocation below could fail because
		# one of the wildcards ("/usr/lib*/libicuio.so" or "/usr/lib*/*/libicuio.so") could end up expanding to
		# a non-existent file name (that would cause bash to treat it as a failure).
		set +e
		ydb_icu_version=$(readlink /usr/lib*/libicuio.so /usr/lib*/*/libicuio.so | sed 's/libicuio.so.\([a-z]*\)\([0-9\.]*\)/\2.\1/;s/\.$//;')
		# Re-enable "set -e" now that "readlink" invocation is done.
		set -e
		export ydb_icu_version

		# Note down if older commit is prior to the YDBOcto#275 commit when NULL and empty string began to be
		# treated the same. This will be used later to skip a few tests.
		pre_octo275_commit="babc2e2e78eb00813cb5d76a8f2bbda66742c1b7"	# 1 commit before the #275 commit
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo275_commit
		is_post_octo275_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to the YDBOcto#649 commit. This will be used later to skip a few tests.
		pre_octo649_commit="9c64861100d7f6c6653a75f7b06f036465c2f486"	# 1 commit before the #649 commit
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo649_commit
		is_post_octo649_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to the YDBAIM implementation commit
		# If older commit, then we won't have an aim.dat, and we need to create the
		# segment, region and data file
		pre_ydbaim_commit="809ed8d862be726c80a4ef1f4f43bebac1e9bc7b"    # 1 commit before the AIM commit
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_ydbaim_commit
		is_post_ydbaim_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to the full implementation of #509
		# It was partially implemented in 74f64658ca9f7da60e9c85a58626a4ac6aef7667
		# Then finished in bc0780f0556969d010767d1b2ed4bdb735e3dddf
		# The TDRC01,TDRC02 sql files fail with the partial implementation, so we can't run them in autoupgrade as they will error out
		pre_octo509_commit="c3137a6c367ce6e987607511358a057446ee2e5c"   # 1 commit before full #509 implementation
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo509_commit
		is_post_octo509_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to all YDBOcto#759 fixes (spread across many commits).
		# This will be used later to skip a few tests.
		pre_octo759_commit="bc3505d790d8e50cc4776863b72673574658ef8d"	# 1 commit before most recent YDBOcto#759 commit
										# (i.e. 060597ef284c2e1cd8f581124b935aba26a63d24)
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo759_commit
		is_post_octo759_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to YDBOcto#993 fix.
		# This will be used later to skip a few tests.
		pre_octo993_commit="fef2307f1488053c7c097f91f3ee822f013b048d"	# 1 commit before most recent YDBOcto#993 fix
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo993_commit
		is_post_octo993_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# Note down if older commit is prior to YDBOcto#211 TAU related fix.
		# This will be used later to skip a few tests.
		pre_octo211_commit="9fe6a0ad1cf9a258171b2e2ec5b18113403656f4"	# 1 commit before YDBOcto#211 commit
		# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
		set +e
		git merge-base --is-ancestor $commitsha $pre_octo211_commit
		is_post_octo211_commit=$?
		# Re-enable "set -e" now that "git merge-base" invocation is done.
		set -e

		# When ydb_chset=UTF-8 is randomly chosen by the test framework, replaying the TLL* subtests in the
		# test_long_lines test requires more stack space than the default of 8Mb. "tests/test_long_lines.bats.in"
		# already has code in the "setup()" function to take this into account. Duplicate that logic here so
		# we don't run into stack space issues (which can show up as a "Segmentation fault (core dumped)" error).
		ulimit -s 131072

		# Point src to newsrc
		ln -s newsrc src
		for tstdir in bats-test.*
		do
			cd $tstdir
			if [[ ! -e $gldfile || ! -e $defaultdat || ! -e $octodat ]]; then
				# This test directory does not contain a 2-region octo setup. auto-upgrade cannot be tested here. Skip.
				# Okay to have AIM region not listed here
				echo "SKIPPED : $tstdir : Does not contain $gldfile or $defaultdat or $octodat" >> ../bats_test.txt
				cd ..
				rm -rf $tstdir
				continue
			fi
			if ! ls ./*.sql 1> /dev/null 2>&1; then
				# This test directory does not contain any "*.sql" files. Skip auto-upgrade test.
				echo "SKIPPED : $tstdir : Does not contain *.sql files" >> ../bats_test.txt
				cd ..
				rm -rf $tstdir
				continue
			fi
			subtest=$(sed 's/.*subtest \[//;s/].*//;' bats_test.out)
			if [[ ($subtest =~ ^"TAU") ]]; then
				if [[ ($subtest =~ ^"TAU002") && (0 == $is_post_octo211_commit) ]]; then
					# Before "9fe6a0ad1cf9a258171b2e2ec5b18113403656f4", TAU002 subtest ran auto-upgrade test
					# queries in old commit itself unintentionally. This caused the auto-upgrade test queries
					# to fail when run by the below code. Therefore skip this subtest.
					echo "SKIPPED : $tstdir : [subtest : $subtest]" >> ../bats_test.txt
					cd ..
					rm -rf $tstdir
					continue
				fi
				# Run auto upgrade specific test
				setup_before_running_sql_files
				subtestName=$(echo $subtest | cut -d " " -f1)
				mv output.txt ${subtestName}_1_output.txt
				[[ -e ${subtestName}_2.sql ]]
				# Disable the "set -e" setting temporarily as we want to check the error reported by Octo
				set +e
				../newsrc/octo -f ${subtestName}_2.sql > output.txt 2>&1
				# In case the older commit had copied the _2.ref file, overwrite it with the newer commit version.
				# This is because it is possible the reference file might have changed in later commits
				# (e.g. an error message format might be different etc.) and we want to use the latest version.
				# Hence the use of "cp -f" below.
				cp -f ../../tests/outref/${subtestName}_2.ref .
				diff ${subtestName}_2.ref output.txt > ${subtestName}_2_output.diff
				set -e
				if [[ -s ${subtestName}_2_output.diff ]]; then
					echo "ERROR : [diff ${subtestName}_2.ref output.txt] returned non-zero diff" | tee -a ../errors.log
					echo "[cat ${subtestName}_2_output.diff] output follows" | tee -a ../errors.log
					tee -a < ${subtestName}_2_output.diff ../errors.log
					exit_status=1
				fi
				cd ..
				continue
			fi
			# ----------------------------------------------------------------------------
			# Exclude specific set of subtests (reasons explained below)
			# ----------------------------------------------------------------------------
			# 1) The TC011 subtest used to have invalid queries which issue errors in later commits
			#    For example, "CREATE TABLE names (id NUMERIC(16,18) PRIMARY KEY ..." used to work fine before
			#    But would issue a "ERR_NUMERIC_SCALE" error due to the YDBOcto#636 fixes.
			#    Therefore, the output of octo against these queries using an older commit and the current commit
			#    could be different (depending on the randomly chosen older commit). And that in turn would
			#    cause the "test-auto-upgrade" pipeline job to signal a false failure. Therefore skip this subtest.
			# 2) The TPC019 subtest runs the query only with "rocto" and expects a ERR_ROCTO_QUERY_TOO_LONG error.
			#    The same error does not happen with "octo" (which means the query will run further along and
			#    terminate abnormally due to not enough "stacksize" limit) which is what this test will use.
			#    Therefore skip this subtest.
			# 3) The TQG03 subtest randomly chooses "nullcharnames" schema and before the YDBOcto#275 commit,
			#    it could use the "NULLCHAR" keyword in the "CREATE TABLE" command which is no longer supported
			#    after the YDBOcto#275 commit. Therefore the auto upgrade would issue a syntax error in that case
			#    as it sees an unsupported NULLCHAR keyword in the "NULLCHARNAMES" table's text definition.
			#    Therefore skip this subtest if the random older commit is prior to the YDBOcto#275 commit.
			# 4) All the TQG* subtests (TQG01, TQG02, TQG03, TQG04, TQG05, TQG06 as of now) could use the empty
			#    string in their queries if the older commit was prior to the YDBOcto#275 commit. Those queries
			#    could give different output in the newer build since the empty string will now be treated as NULL.
			#    Therefore, skip all TQG* subtests if the random older commit is prior to the YDBOcto#275 commit.
			#    Note that (3) is subsumed by (4) so just one check is needed below for both those cases.
			# 5) By a similar reasoning as (4), the TJC004/TJC005/TJC006/TJC007 subtests in the
			#    "test_jdbc_connection" test use the same query generator that the "TQG*" subtests use.
			#    And so need to be skipped for the same conditions as TQG*.
			# 6) By a similar reasoning, the TC027/TC033/TC034 subtests (in the "test_createtable" bats test) used
			#    "NULLCHAR" previously and so have to be skipped.
			# 7) The "TLQ02" subtest runs a large query which require a lot of memory when rerun in the
			#    test-auto-upgrade job (VmSize value in /proc/PID/status goes as high as 3.3GiB) and causes the
			#    process to get "Killed" in the gitlab pipelines (likely the OOM killer kicks in) so skip this.
			#    Note though that as part of YDBOcto#649, this query was reduced 10x and so we need to skip only
			#    if the random prior commit is older than the #649 commit.
			# 8) The TDRC01,TDRC02 sql files fail with the partial implementation of Octo#509, so we can't run them
			#    in an autoupgrade as they will error out
			# 9) The TQG02 subtest failed once due to different number of rows returned for a query after the
			#    YDBOcto#759 fixes compared to a commit before the fixes. Similar issues are expected to be there
			#    in other TQG* subtests as well so skip all TQG* tests in case the older commit predates the
			#    YDBOcto#759 fixes.
			# 10) Output of update queries are not easily comparable when JDBC driver is used to execute TQG* and TJC*
			#     subtests. Skip these tests when files of the name `tabledefinition` are seen in the test directory as
			#     they are only present when update queries are generated by the `QueryGenerator.m`.
			# ----------------------------------------------------------------------------
			skip_test=0
			if [[ ($subtest =~ "TC011 : ") || ($subtest =~ "TPC019 : ")                    \
					|| (($subtest =~ ^"TQG") && (0 == $is_post_octo275_commit))    \
					|| (($subtest =~ ^"TJC004") && (0 == $is_post_octo275_commit)) \
					|| (($subtest =~ ^"TJC005") && (0 == $is_post_octo275_commit)) \
					|| (($subtest =~ ^"TJC006") && (0 == $is_post_octo275_commit)) \
					|| (($subtest =~ ^"TJC007") && (0 == $is_post_octo275_commit)) \
					|| (($subtest =~ ^"TC027") && (0 == $is_post_octo275_commit))  \
					|| (($subtest =~ ^"TC033") && (0 == $is_post_octo275_commit))  \
					|| (($subtest =~ ^"TC034") && (0 == $is_post_octo275_commit))  \
					|| (($subtest =~ ^"TLQ02") && (0 == $is_post_octo649_commit))  \
					|| (($subtest =~ ^"TDRC01") && (0 == $is_post_octo509_commit)) \
					|| (($subtest =~ ^"TDRC02") && (0 == $is_post_octo509_commit)) \
					|| (($subtest =~ ^"TQG") && (0 == $is_post_octo759_commit))    \
					|| (($subtest =~ ^"TSCP25") && (0 == $is_post_octo993_commit)) \
				]]; then
				skip_test=1
			else
				# Skip execution if JDBC driver is used and update queries are present in the test
				# as JDBC driver output is not easily comparable.
				tabledefsqlfile=""
				if [[ ($subtest =~ ^"TQG") ]]; then
					qgquery=1
					# Disable the "set -e" setting temporarily as below command can return exit status 0 or 1
					set +e
					tabledefsqlfile=$(compgen -G "tabledefinition-TQG[0-9][0-9]\.sql")
					set -e
				elif [[ ($subtest =~ ^TJC00[4-7]) ]]; then
					qgquery=1
					# Disable the "set -e" setting temporarily as below command can return exit status 0 or 1
					set +e
					tabledefsqlfile=$(compgen -G "tabledefinition-TJC00[4-7]\.sql")
					set -e
				else
					qgquery=0
				fi
				# A few older commits had env vars logged in the file "env.out" but later commits changed it
				# to "dbg_env.out" so allow for either of those files since we don't know which older commit
				# got randomly picked.
				if [[ ! -e dbg_env.out ]]; then
					# "dbg_env.out" does not exist. This means it is an older commit that stored env vars in
					# the file "env.out". So rename it to "dbg_env.out" just like more recent commits would do.
					mv env.out dbg_env.out
				fi
				if [[ "" != $tabledefsqlfile ]]; then
					if grep -q "randclient:JDBC" dbg_env.out; then
						skip_test=1
					fi
				fi
			fi
			if [[ 1 -eq $skip_test ]]; then
				echo "SKIPPED : $tstdir : [subtest : $subtest]" >> ../bats_test.txt
				cd ..
				rm -rf $tstdir
				continue
			fi

			setup_before_running_sql_files
			if [[ "" != $tabledefsqlfile ]]; then
				# This is a test_query_generator test. Run table definition queries prior to running
				# other queries as other queries will depend on the tables created in this definition.
				outfile="autoupgrade.$tabledefsqlfile.out"
				# To ignore errors seen by execution of table-definition queries || true is added to the command
				# below. Errors are only expected during the execution of INSERT statement in table-defintion file.
				# The failure will be mostly because of constraint violations. Any other error
				# will result in rest of the test to fail and is expected to be back tracked to this point.
				../newsrc/octo -f $tabledefsqlfile > $outfile 2>&1 || true
			fi
			# TEST1 and TEST2 below together test that Octo automatically recreates any
			# binary-definitions/plans/xrefs/triggers as needed thereby testing YDBOcto#90.
			errors_found=0
			for sqlfile in *.sql
			do
				# .sql files with _comment- are only seen when JDBC client is used and they are not part of test queries
				if [[ ($sqlfile =~ "_comment-") ]]; then
					continue
				fi
				# TEST1
				# We do not want any failures in the "octo" invocation below to exit the script.
				# So disable the "set -e" setting temporarily for this step.
				set +e
				outfile="autoupgrade.$sqlfile.out"
				../newsrc/octo -f $sqlfile > $outfile 2>&1
				ret_status=$?
				# Re-enable "set -e" now that "octo" invocation is done.
				set -e
				if [[ 1 -eq $qgquery ]]; then
					querytype=$(awk 'NR==1 {print tolower($1);}' $sqlfile)
					# Ignore any errors in insert or update or delete query execution as it will be validated
					# against previous commit output error message down below.
					if [[ (("insert" == $querytype) || ("update" == $querytype) || ("delete" == $querytype)) ]]; then
						ret_status=0
					fi
				fi
				if [[ 1 -lt $ret_status ]]; then
					# Invoking newer build of Octo on environment set up by older Octo resulted in a
					# non-zero exit status greater than 1. This most likely means a fatal error like a SIG-11 or
					# Assert failure etc. Octo exits with 0 for success and 1 when an error is encountered,
					# while asserts exit with 250 and SIG-11s with 245. Since we are only testing auto-upgrade
					# behavior here, we can accept an exit code of 1, as this will be issued only for failing
					# queries, which are expected in some test cases as of this writing. For all other error
					# codes, fail and record the error.
					echo " ERROR : [newsrc/octo -f $tstdir/$sqlfile] > autoupgrade.$sqlfile.out : Exit status = $ret_status" | tee -a ../errors.log
					echo " ERROR :   --> It is likely that bumping up FMT_BINARY_DEFINITION would fix such failures" | tee -a ../errors.log
					exit_status=1
					errors_found=1
				fi
				# If this is a test output directory for the "test_query_generator" test, then do additional
				# testing of actual output. We expect the output to be identical between the older commit and
				# the current commit even though the current commit reused binary table/function defnitions and
				# plans/triggers/xrefs generated by the older commit. We can do actual output verification of the
				# "test_query_generator" test because we know this test validates Octo's output against Postgres
				# and we do not expect any errors in the output of Octo using either the older or newer commit.
				#
				# Sometimes TQG* subtests store the octo output in files of the form (if they go through JDBC driver)
				#	TQG06-0_nocomment-000.sql.octo.out
				# Sometimes they store it in files of the form (if they do not go through JDBC driver)
				#	TQG01-0-000.octo.out
				# Handle both cases below.
				octooutfile=""
				if [[ -e $sqlfile.octo.out ]]; then
					octooutfile="$sqlfile.octo.out"
					usedjdbcdriver=1
				else
					# shellcheck disable=SC2001
					basename=$(echo $sqlfile | sed 's/\.sql//g')
					if [[ -e $basename ]]; then
						octooutfile=$basename
					fi
					usedjdbcdriver=0
				fi
				if [[ ($subtest =~ ^"TQG") && (-n $octooutfile) ]]; then
					# a) If the query executed is CREATE/DROP statement and JDBC driver was used, the output will not be
					#    comparable so just ensuring ret_status was -lt 1 is enough validation(done prior to this code block.
					#    Continue to the next file.
					# b) If the random older commit predates the YDBOcto#649 commit, then the octo output
					#    would not contain the row-header and row summary line at the head and tail of the octo output.
					#    So filter that out from the newer Octo build output.
					# c) If the random older commit postdates the YDBOcto#649 commit, even then the octo output would
					#    not contain the row-header and row summary line if the test ran using the JDBC driver.
					#    So remove the same lines from the octo output of the newer commit.
					if [[ (0 != $usedjdbcdriver) && (("create" == $querytype) || ("drop" == $querytype)) ]]; then
						# JDBC driver would have output the `#number of rows` text and that is not comparable to `../newsrc/octo -f` execution
						continue
					fi
					pre_octo649_commit="9c64861100d7f6c6653a75f7b06f036465c2f486"
					# Disable the "set -e" setting temporarily as the "git merge-base" can return exit status 0 or 1
					set +e
					git merge-base --is-ancestor $commitsha $pre_octo649_commit
					is_post_octo649_commit=$?
					# Re-enable "set -e" now that "git merge-base" invocation is done.
					set -e
					if [[ (0 == $is_post_octo649_commit) || (0 != $usedjdbcdriver) ]]; then
						# If QueryGenerator is used and has generated a tabledefinition file, and the query is of
						# type insert/update/delete, then output of these queries will not have row-header or row
						# summary lines. Remove the row-header and row-summary lines for SELECT query results only.
						if ! [[ ((1 -eq $qgquery) && (("insert" == $querytype) || ("update" == $querytype) || ("delete" == $querytype))) ]]; then
							mv $outfile $outfile.tmp
							tail -n +2 $outfile.tmp | head -n -1 > $outfile
						fi
					fi
					# Error formatting in case of query generator with update queries
					if [[ (1 -eq $qgquery) && (("insert" == $querytype) || ("update" == $querytype) || ("delete" == $querytype)) ]]; then
						# Below sed statements are similar to the sed statements in test_helper.bats.in
						# run_query_in_octo_and_postgres_and_crosscheck(). Any change here must be done
						# there as well.
						if grep -q "randclient:OCTO" dbg_env.out; then
							sed 's/ : \(Failing.*\)//;s/ : \(Key .*\)//;/WARN/d;s/\(Duplicate Key Value violates UNIQUE constraint\).*/\1/;s/^\[ERROR\]: ERR_CHECK_CONSTRAINT_VIOLATION: New row for table \(.*\) violates CHECK constraint.*/\[ERROR\]: ERR_CHECK_CONSTRAINT_VIOLATION: New row for table \U\1\E violates check constraint/;/LINE/d;/\^/d;' $outfile &> $outfile.tmp
						else
							#PSQL
							sed 's/ : \(Failing.*\)//;s/ : \(Key .*\)//;/WARN/d;s/\(Duplicate Key Value violates UNIQUE constraint\).*/\1/;s/^ERROR:  New row for table \(.*\) violates CHECK constraint.*/ERROR:  New row for table \U\1\E violates check constraint/;/LINE/d;/\^/d;' $outfile &> $outfile.tmp
						fi
						mv $outfile.tmp $outfile
					fi
					# TEST2
					# $sqlfile.log is Octo's output for the same query using the older commit build
					# It could contain "null" references if it was run through the JDBC driver.
					# Replace that with the empty string for the below diff since we did not use the JDBC
					# driver for the newer Octo build output.
					reffile="autoupgrade.$sqlfile.ref"
					sed 's/^null$//;s/^null|/|/;s/|\<null\>/|/g' $octooutfile > $reffile
					# Check if the output required sorting. We deduce this from presence of *unsorted* files.
					logfile="autoupgrade.$sqlfile.log"
					if compgen -G "$sqlfile.unsorted.**" > /dev/null; then
						mv $reffile $reffile.unsorted
						sort $reffile.unsorted > $reffile
						sort $outfile > $logfile
					else
						cp $outfile $logfile
					fi
					# Check if the output required only a rowcount check ("-- rowcount-only-check" in query file)
					# This is deduced from the presence of *.diff files. If it is present, then a diff was done.
					# If it is not, a rowcount check was done when the original test ran. Do the same thing below
					# with the newer build of Octo.
					if [[ -e $sqlfile.diff ]]; then
						# .diff file exists. This means an actual diff was done. Do a diff in the new Octo build too.
						difffile="autoupgrade.$sqlfile.diff"
						diff $reffile $logfile > $difffile || true
						if [[ -s $difffile ]]; then
							echo "ERROR : [diff $reffile $logfile] returned non-zero diff. See $difffile for details" | tee -a ../errors.log
							echo "ERROR :   --> It is likely that bumping up FMT_PLAN_DEFINITION would fix such failures" | tee -a ../errors.log
							exit_status=1
							errors_found=1
						fi
					else
						# .diff file does not exist. This means only a rowcount check was done. Do the same check with the newer Octo build output.
						oldoctolines=$(wc -l $reffile | awk '{print $1}')
						newoctolines=$(wc -l $logfile | awk '{print $1}')
						if [[ $oldoctolines -ne $newoctolines ]]; then
							echo "ERROR : [$reffile has $oldoctolines lines but $logfile contains $newoctolines lines]" | tee -a ../errors.log
							exit_status=1
							errors_found=1
						fi
					fi
				fi
			done
			cd ..
			if [[ 0 == $errors_found ]]; then
				# No auto-upgrade related errors found in this bats test directory.
				# Delete this directory before moving on (reduces size of pipeline artifacts in case of failure).
				rm -rf $tstdir
			fi
		done
	else
		# Find out all "CREATE TABLE" queries in tests/fixtures/*.sql. Generate one query file for each.
		# Filter out lines like "\set ON_ERROR_STOP on" that are in tests/fixtures/postgres-*.sql files
		#   as they confuse split_queries.py. Also filter out queries with errors that are in TERR*.sql files
		#   and exclude `mysql-*-.sql` symbolic links from `auto-upgrade` cleanup.
		# Note that some "CREATE TABLE" queries in tests/fixtures/*.sql can be in lower case i.e. "create table"
		#   so search for those too. But convert them to upper case before storing them as a later call to
		#   "split_queries.py" assumes an upper case string "CREATE TABLE".
		grep --exclude=mysql-*.sql -li "CREATE TABLE" ../tests/fixtures/*.sql | grep -v TERR | xargs cat | sed 's/create table/CREATE TABLE/g' | grep -v ON_ERROR_STOP > create_table.sql
		../tests/fixtures/sqllogic/split_queries.py create_table.sql "CREATE TABLE"
		# Create *.gld and *.dat files
		rm -f ./*.gld ./*.dat || true
		# Point src to newsrc so GDE works fine or else ZROSYNTAX error would be issued.
		rm -f src || true; ln -s newsrc src
		export ydb_gbldir="yottadb.gld"
		$ydb_dist/yottadb -run ^GDE <<FILE
		change -region DEFAULT -null_subscripts=true -record_size=1048576
		change -segment DEFAULT -file_name=mumps.dat
		add -region OCTOREG -dyn=OCTOSEG -null_subscripts=true -key_size=1019 -record_size=1048576
		add -segment OCTOSEG -file="octo.dat"
		add -segment AIMSEG -file="aim.dat" -access_method=MM -block_size=2048
		add -region AIMREG -dyn=AIMSEG -nojournal -key_size=1019 -null_subscripts=always -record_size=2048
		add -name %ydbocto* -region=OCTOREG
		add -name %ydbAIM* -region=AIMREG
FILE
		rm ./*.dat || true
		$ydb_dist/mupip create
		touch errors.log
		filediff() {
			local filename1="$1"
			local filename2="$2"
			local filename=""
			for dir in oldsrc newsrc
			do
				if [[ "oldsrc" == $dir ]]; then
					filename=$filename1
				else
					filename=$filename2
				fi
				# Replace variable parts of output (e.g. date/time/full-path-of-directory etc.)
				sed -i 's/[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}/DATE/g' ../$dir/$filename
				sed -i 's/[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}/TIME/g' ../$dir/$filename
				sed -i 's/'$dir'/##SRCDIR##/g' ../$dir/$filename
				# Delete DBFILEXT messages as they contain pid and other variable output
				sed -i '/DBFILEXT/d' ../$dir/$filename
			done
			diff ../oldsrc/$filename1 ../newsrc/$filename2 > $filename2.diff || true
			if [[ -s $filename2.diff ]]; then
				echo "ERROR : [diff oldsrc/$filename1 newsrc/$filename2] returned non-zero diff" | tee -a ../errors.log
				echo "[cat $filename2.diff] output follows" | tee -a ../errors.log
				tee -a < $filename2.diff ../errors.log
				exit_status=1
			fi
		}
		run_octo() {
			local queryfile=$1
			local dir=$2
			# We do not want any failures in the "octo" invocation below to exit the script.
			# So disable the "set -e" setting temporarily for this step.
			set +e
			if [[ $3 == "vv" ]]; then
				./octo -vv -f $queryfile >& $queryfile.vv.out	# Need -vv to figure out generated M plan name
				# See comment in error block for TEST1 above for why exit code of 1 is accepted
				if [[ $? -gt 1 ]]; then
					echo "ERROR : [octo -f $dir/$queryfile] returned non-zero exit status : $?" | tee -a ../errors.log
					exit_status=1
				fi
				# It is possible that the CREATE TABLE query is expected to error out (e.g. the test where
				# this query was picked from is testing an error code path). That is why we cannot issue
				# an error if we see an exit code of 1 from the "octo" invocation above. But we have seen
				# "%YDB-E-DLLCHSETM" errors previously and those are unexpected in all cases and are indicative
				# of issues in this script (i.e. "build.sh") because of which real code issues can be masked out.
				# Therefore check explicitly for that error and if so issue an error from this script.
				if grep -q "%YDB-E-DLLCHSETM" $queryfile.vv.out; then
					echo "ERROR : [octo -f $dir/$queryfile] encountered unexpected %YDB-E-DLLCHSETM error" | tee -a ../errors.log
					exit_status=1
				fi
				plan_name=$(grep _ydboctoP $queryfile.vv.out | sed 's/.*_ydboctoP/_ydboctoP/;s/].*//')
			fi
			./octo -f $queryfile >& $queryfile.out	# Run without -vv to get actual output (minus INFO/LP_ etc. output)
								# This will be used for filediff as it is deterministic.
			# Re-enable "set -e" now that "octo" invocation is done.
			set -e
		}
		cd oldsrc
		export ydb_routines=". $ydb_routines $ydb_dist/libyottadbutil.so"
		cp ../*.gld ../{mumps,octo}.dat ../create_table-*.sql .
		cp ./*.gld {mumps,octo}.dat create_table-*.sql ../newsrc
		for queryfile in create_table-*.sql
		do
			echo " --> Processing $queryfile"
			# Run the "CREATE TABLE" query in oldsrc directory to create the binary table definition
			cd ../oldsrc; rm -f ../src || true; ln -s oldsrc ../src
			run_octo $queryfile oldsrc novv
			# Determine table name and generate a query that selects all columns from that table
			tablename=$(grep -n "CREATE TABLE" $queryfile | grep -v "^--" | sed 's/.*CREATE TABLE //;s/(.*//;' | awk '{print $1}')
			echo "select * from $tablename;" > $queryfile.2
			# Copy over database files from oldsrc to newsrc
			cp {mumps,octo}.dat $queryfile.2 ../newsrc
			# Run the SELECT query in oldsrc directory
			run_octo $queryfile.2 oldsrc vv	# sets "plan_name" variable due to "vv"
			old_plan_name=$plan_name
			# Run an empty query file in the newsrc directory to force an auto upgrade of the binary table definitions
			cd ../newsrc; rm -f ../src || true; ln -s newsrc ../src
			echo "" > $queryfile.null
			run_octo $queryfile.null newsrc novv
			# Run the SELECT query in newsrc directory
			run_octo $queryfile.2 newsrc vv	# sets "plan_name" variable due to "vv"
			new_plan_name=$plan_name
			# Compare SELECT query output between oldsrc and newsrc. Should be none.
			# Allow for "[ERROR]" in $queryfile.2.out
			# But we also expect the exact same output with auto upgrade forced.
			filediff $queryfile.2.out $queryfile.2.out
			if [[ $old_plan_name != $new_plan_name ]]; then
				echo "ERROR : $queryfile : Plan name in oldsrc [$old_plan_name] differs from newsrc [$new_plan_name]" | tee -a ../errors.log
				exit_status=1
			fi
			if [[ ("" != "$new_plan_name") && ("" != "$old_plan_name") ]]; then
				# "$new_plan_name" can be "" for example if "CREATE TABLE" occurs in a comment in the query
				# file and the actual query is something else like a "DROP TABLE". In that case, skip this check.
				filediff "$old_plan_name" "$new_plan_name"
			fi
			# Need to DROP TABLE the current table in "oldsrc" directory to avoid reusing this plan for future
			# CREATE TABLE queries that point to the same table name (e.g. `Customers` vs `customers`) as that
			# can cause a failure in the pipeline job (see commit message for details).
			cd ../oldsrc; rm -f ../src || true; ln -s oldsrc ../src
			echo "DROP TABLE $tablename;" > "$queryfile.drop"
			run_octo "$queryfile.drop" oldsrc novv
			cd ../newsrc; rm -f ../src || true; ln -s newsrc ../src
		done
		cd ..
	fi
	# Set verbose mode back now that for loop is over (so we see each command as it gets executed)
	set -v
	set -x
fi

echo " -> exit $exit_status"
# Unset verbose mode before printing summary of failure results if any
set +x
set +v

if [[ 0 != "$exit_status" ]]; then
	if [[ "test-auto-upgrade" != "$jobname" ]]; then
		echo "# ----------------------------------------------------------"
		echo "# List of failed tests/subtests and their output directories"
		echo "# ----------------------------------------------------------"
		if ! [[ -s Testing/Temporary/LastTest.log ]]; then
			echo "# Detected script failure prior to BATS test execution. Please review script output to determine source."
		else
			grep -A 32 -E "not ok|Test: " Testing/Temporary/LastTest.log | grep -E "not ok|# Temporary|Test: " | grep -C 1 "not ok" | sed "s/^not/  &/;s/^#/  &/"
		fi
		echo "# -----------------------------"
	else
		echo "# ----------------------------------------------------------"
		echo "# List of errors (cat errors.log)"
		echo "# ----------------------------------------------------------"
		grep ERROR errors.log
	fi
fi

exit $exit_status
