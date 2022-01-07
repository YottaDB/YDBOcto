#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
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
set(YDBOCTO_MINOR_VERSION 1)
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
option(ENABLE_ASAN "Enable address sanitizer in build?" OFF)
option(ENABLE_AUTO_VAR_INIT_PATTERN "Enable -ftrivial-auto-var-init=pattern in build?" OFF)

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
# Check if YottaDB is compiled using ASAN. If so, enable ASAN by default
execute_process(COMMAND nm $ENV{ydb_dist}/libyottadb.so
	COMMAND grep -c "U __asan_init"
	OUTPUT_VARIABLE YDBASAN)
if(${YDBASAN} GREATER_EQUAL 1)
	set(ENABLE_ASAN ON)
	message(STATUS "Forcing Address Sanitizer usage as YottaDB is compiled using Address Sanitizer")
endif()
if(${ENABLE_ASAN})
	message(STATUS "All Octo operations will have Address Santizer enabled")
endif()
if(${ENABLE_AUTO_VAR_INIT_PATTERN})
	message(STATUS "All files will be compiled with -ftrivial-auto-var-init=pattern")
endif()

if(NOT ${DISABLE_INSTALL})
	message(STATUS "Install destination: ${CMAKE_INSTALL_PREFIX}")
endif()

set(OCTO_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools/")
set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")

if(NOT MAX_EXPRESSION_LENGTH)
	set(MAX_EXPRESSION_LENGTH 8192)
endif()

if(NOT MAX_KEY_COUNT)
	set(MAX_KEY_COUNT 255)
endif()

include(CTest)
add_subdirectory(src)

# Sanity check to ensure that required variables are set
if(${TEST_VISTA} AND NOT EXISTS ${TEST_VISTA_ENV_FILE})
	message(FATAL_ERROR "You must set TEST_VISTA_ENV_FILE (e.g. '~vehu/etc/env') if you set TEST_VISTA")
endif()
