cmake_minimum_required (VERSION 2.8)
project(OctoSQL)
set(OCTO_SQL_MAJOR_VERSION 0)
set(OCTO_SQL_MINOR_VERSION 2)
set(CMAKE_BUILD_TYPE Debug)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/;${CMAKE_SOURCE_DIR}/ydbcmake/")
set(OCTO_TOOLS_DIR "${CMAKE_SOURCE_DIR}/tools/")
set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")

if(NOT STRING_BUFFER_LENGTH)
	set(STRING_BUFFER_LENGTH 32768)
endif()

if(NOT MAX_EXPRESSION_LENGTH)
	set(MAX_EXPRESSION_LENGTH 8192)
endif()

if(NOT MAX_KEY_COUNT)
	set(MAX_KEY_COUNT 255)
endif()

#enable_testing()
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

	# configured documentation tools and intermediate build results
	set(BINARY_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/_build")

	# Sphinx cache with pickled ReST documents
	set(SPHINX_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/_doctrees")

	# HTML output directory
	set(SPHINX_HTML_DIR "${CMAKE_CURRENT_BINARY_DIR}/html")

	configure_file(
	    "${CMAKE_CURRENT_SOURCE_DIR}/doc/conf.py.in"
	    "${BINARY_BUILD_DIR}/conf.py"
	    @ONLY)

	add_custom_target(docs
	    ${SPHINX_EXECUTABLE}
		-q -b html
		-c "${BINARY_BUILD_DIR}"
		-d "${SPHINX_CACHE_DIR}"
		"${CMAKE_CURRENT_SOURCE_DIR}/doc"
		"${SPHINX_HTML_DIR}"
	    COMMENT "Building HTML documentation with Sphinx")
endif()
