#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <http://unlicense.org/>

#  Readline_FOUND - System has YottaDB
#  Readline_INCLUDE_DIRS - The YottaDB include directories
#  Readline_LIBRARIES - The libraries needed to use YottaDB

find_path(Readline_ROOT_DIR
  NAMES readline/readline.h)


find_path(Readline_INCLUDE_DIR NAMES readline.h
  HINTS ${Readline_ROOT_DIR}/readline)

find_library(Readline_LIBRARY NAMES readline
             HINTS ${Readline_ROOT_DIR})

set(Readline_LIBRARIES ${Readline_LIBRARY})
set(Readline_INCLUDE_DIR ${Readline_INCLUDE_DIR})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Readline  DEFAULT_MSG
                                  Readline_LIBRARY Readline_INCLUDE_DIR)

mark_as_advanced(Readline_INCLUDE_DIR Readline_LIBRARIES)
