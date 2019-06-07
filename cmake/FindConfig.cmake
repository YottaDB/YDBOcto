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

FIND_PATH(CONFIG_INCLUDE_DIR libconfig.h /usr/include /usr/local/include)

FIND_LIBRARY(CONFIG_LIBRARY NAMES config PATH /usr/lib /usr/local/lib)

IF (CONFIG_INCLUDE_DIR AND CONFIG_LIBRARY)
    SET(CONFIG_FOUND TRUE)
ENDIF ( CONFIG_INCLUDE_DIR AND CONFIG_LIBRARY)

IF (CONFIG_FOUND)
    IF (NOT CONFIG_FIND_QUIETLY)
	MESSAGE(STATUS "Found Config: ${CONFIG_LIBRARY}")
    ENDIF (NOT  CONFIG_FIND_QUIETLY)
ELSE(CONFIG_FOUND)
    IF (Config_FIND_REQUIRED)
	IF(NOT CONFIG_INCLUDE_DIR)
	    MESSAGE(FATAL_ERROR "Could not find LibConfig header file!")
	ENDIF(NOT CONFIG_INCLUDE_DIR)

	IF(NOT CONFIG_LIBRARY)
	    MESSAGE(FATAL_ERROR "Could not find LibConfig library file!")
	ENDIF(NOT CONFIG_LIBRARY)
    ENDIF (Config_FIND_REQUIRED)
ENDIF (CONFIG_FOUND)
