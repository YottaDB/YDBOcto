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

set(CMAKE_M_CREATE_SHARED_LIBRARY "<CMAKE_C_COMPILER> -shared -o <TARGET> <OBJECTS>")
set(CMAKE_M_CREATE_SHARED_MODULE "<CMAKE_C_COMPILER> <CMAKE_SHARED_LIBRARY_C_FLAGS> -o <TARGET> <OBJECTS>")
set(CMAKE_M_CREATE_STATIC_LIBRARY "")

# Option to suppress mumps compiler warnings
option(M_NOWARNING "Disable warnings and ignore status code from M compiler")
option(M_EMBED_SOURCE "Embed source code in generated shared object" OFF)
option(M_DYNAMIC_LITERALS "Enable dynamic loading of source code literals" OFF)
option(M_NOLINE_ENTRY "Compile M code without access to label offsets" OFF)

set(CMAKE_M_COMPILE_OBJECT "LC_ALL=\"${LC_ALL}\" ydb_chset=\"${ydb_chset}\" ydb_icu_version=\"${icu_version}\" <CMAKE_M_COMPILER> -object=<OBJECT>")

if(M_EMBED_SOURCE)
  set(CMAKE_M_COMPILE_OBJECT "${CMAKE_M_COMPILE_OBJECT} -embed_source")
endif()

if(M_DYNAMIC_LITERALS)
  set(CMAKE_M_COMPILE_OBJECT "${CMAKE_M_COMPILE_OBJECT} -dynamic_literals")
endif()

if(M_NOLINE_ENTRY)
  set (CMAKE_M_COMPILE_OBJECT "${CMAKE_M_COMPILE_OBJECT} -noline_entry")
endif()

if(M_NOWARNING)
  set(CMAKE_M_COMPILE_OBJECT "${CMAKE_M_COMPILE_OBJECT} -nowarning <SOURCE> || true")
else()
  set(CMAKE_M_COMPILE_OBJECT "${CMAKE_M_COMPILE_OBJECT} <SOURCE>")
endif()

set(CMAKE_M_LINK_EXECUTABLE "")

set(CMAKE_M_OUTPUT_EXTENSION .o)
