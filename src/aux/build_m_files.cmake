#################################################################
#								#
# Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# This file has to be used with include(), instead of the preferred
# add_subdirectory(), because we want to keep the created objects in the same
# directory (src) that is including this file.
#
# Create a shared library of M routines
set(aux_files
  aux/_ydboctoAdmin.m
  aux/_ydboctoCleanup.m
  aux/_ydboctoDiscard.m
  aux/_ydboctofCONCAT.m
  aux/_ydboctofCURRENTTIME.m
  aux/_ydboctofCURRENTTIMESTAMP.m
  aux/_ydboctofDATEFORMAT.m
  aux/_ydboctofLOCALTIME.m
  aux/_ydboctofLPAD.m
  aux/_ydboctofTRUNCATE.m
  aux/_ydboctoInit.m
  aux/_ydboctoUtils.m
  aux/_ydboctopgfunctions.m
  aux/_ydboctoZinterrupt.m
  aux/_ydboctoplanhelpers.m
  aux/_ydboctoselect.m
  aux/_ydboctosqlfunctions.m
  aux/_ydboctoxrefupdate.m
  aux/_ydboctoViewsUpgrade.m
  aux/_ydboctofTODTFORMAT.m
)

add_ydb_library(_ydbocto SOURCES ${aux_files})

if(NOT ${DISABLE_INSTALL})
	# Library
	install_ydb_library(_ydbocto)
	# M Routines
	install_ydb_sources(SOURCES ${aux_files})
endif()
