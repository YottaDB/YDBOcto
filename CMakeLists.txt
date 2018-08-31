cmake_minimum_required (VERSION 2.8)
project(OctoSQL)
set(OCTO_SQL_MAJOR_VERSION 0)
set(OCTO_SQL_MINOR_VERSION 1)
set(CMAKE_BUILD_TYPE Debug)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

if(NOT STRING_BUFFER_LENGTH)
	set(STRING_BUFFER_LENGTH 4096)
endif()

if(NOT MAX_EXPRESSION_LENGTH)
	set(MAX_EXPRESSION_LENGTH 1024)
endif()

enable_testing()
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
