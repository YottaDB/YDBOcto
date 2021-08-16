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

cmake_minimum_required (VERSION 3.0)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/;${CMAKE_SOURCE_DIR}/ydbcmake/")

# Find YottaDB
find_package(YOTTADB REQUIRED)

# Do this unconditionally, as our cmake plugin double-checks our values to
# ensure correctness; i.e. if you set a bad ydb_dist, our plugin will set the
# correct value. You must double check it yourself in the status messages.
set(ENV{ydb_dist} ${YOTTADB_INCLUDE_DIRS})
message(STATUS "$ydb_dist: $ENV{ydb_dist}")

project(YDBOcto C M)
set(YDBOCTO_MAJOR_VERSION 1)
set(YDBOCTO_MINOR_VERSION 0)
set(YDBOCTO_PATCH_VERSION 0)

set(MIN_YDB_RELEASE "1.29")
execute_process(
	COMMAND ${PROJECT_SOURCE_DIR}/tools/get_ydb_release.sh ${MIN_YDB_RELEASE}
	OUTPUT_VARIABLE YDB_RELEASE_STRING
	RESULT_VARIABLE version_accepted
	WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/tests/fixtures
)

if(NOT version_accepted)
	message(STATUS "YottaDB version: r${YDB_RELEASE_STRING}")
else()
	message(FATAL_ERROR "YottaDB version r${YDB_RELEASE_STRING} not supported. Minimum supported version: ${MIN_YDB_RELEASE}")
endif()

if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE RelWithDebInfo)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

set(CMAKE_INSTALL_PREFIX "$ENV{ydb_dist}/plugin")

option(DISABLE_INSTALL "Disable Installation?" OFF)
option(FULL_TEST_SUITE "Run full test suite?" OFF)
option(TEST_SPEED "Run separate speed test?" OFF)
option(TEST_VISTA "Run separate VistA test (Database needs to be set-up separately)?" OFF)
if(NOT ${DISABLE_INSTALL})
	message(STATUS "Install destination: ${CMAKE_INSTALL_PREFIX}")
endif()
if(NOT ${FULL_TEST_SUITE})
	message(STATUS "Test suite run by `make test`: Limited")
else()
	message(STATUS "Test suite run by `make test`: Full")
endif()
if(${TEST_SPEED})
	message(STATUS "`make test` will run speed test")
endif()
if(${TEST_VISTA})
	message(STATUS "`make test` will run VistA tests")
endif()

set(OCTO_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools/")
set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")

if(NOT MAX_EXPRESSION_LENGTH)
	set(MAX_EXPRESSION_LENGTH 8192)
endif()

if(NOT MAX_KEY_COUNT)
	set(MAX_KEY_COUNT 255)
endif()

# This tells valgrind to use `ignored-valgrind-errors.supp` as the list of errors to ignore when
# running Octo.
# NOTE: `CACHE` is required or CMake will not generate the right `MemoryCheckSuppressionFile` line
# in `DartConfiguration.tcl` (similar to `export` in shell). See
# https://cmake.org/cmake/help/latest/manual/cmake-language.7.html#cmake-language-variables for
# more information.
set(MEMORYCHECK_SUPPRESSIONS_FILE ${PROJECT_SOURCE_DIR}/tools/ci/ignored-valgrind-errors.supp CACHE STRING "")
include(CTest)
add_subdirectory(src)
