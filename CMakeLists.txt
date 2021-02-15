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

project(YDBOcto C M)
set(YDBOCTO_MAJOR_VERSION 1)
set(YDBOCTO_MINOR_VERSION 0)
set(YDBOCTO_PATCH_VERSION 0)
if(DEFINED ENV{ydb_dist})
	message(STATUS "$ydb_dist: $ENV{ydb_dist}")
else()
	message(FATAL_ERROR "The $ydb_dist environment variable is not set. Please set $ydb_dist before building and installing Octo")
endif()

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
if(NOT DISABLE_INSTALL)
	set(DISABLE_INSTALL "OFF")
endif()
if(${DISABLE_INSTALL} MATCHES "OFF")
	message(STATUS "Install destination: ${CMAKE_INSTALL_PREFIX}")
endif()
if(NOT FULL_TEST_SUITE)
	set(FULL_TEST_SUITE "OFF")
	message(STATUS "Test suite run by `make test`: Limited")
else()
	message(STATUS "Test suite run by `make test`: Full")
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

# Stuff related to documentation building
find_package(Sphinx)
if(Sphinx_FOUND)
	if(NOT DEFINED SPHINX_THEME)
	    set(SPHINX_THEME default)
	endif()

	if(NOT DEFINED SPHINX_THEME_DIR)
	    set(SPHINX_THEME_DIR)
	endif()

	# Sphinx source directory
	set(SPHINX_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/doc")

	# Sphinx cache with pickled ReST documents
	set(SPHINX_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/_doctrees")

	# HTML output directory
	set(SPHINX_HTML_DIR "${CMAKE_CURRENT_BINARY_DIR}/html")

	add_custom_target(docs
	    ${SPHINX_EXECUTABLE}
		-q -b html
		-c "${SPHINX_SOURCE_DIR}"
		-d "${SPHINX_CACHE_DIR}"
		"${SPHINX_SOURCE_DIR}"
		"${SPHINX_HTML_DIR}"
	    COMMENT "Building HTML documentation with Sphinx")
endif()
