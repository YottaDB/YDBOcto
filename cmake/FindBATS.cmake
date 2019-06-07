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

# - Finds BAT instalation
# This module sets up BAT information
# It defines:
# BAT_FOUND          If the BAT is found
find_program(BATS bats)

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set BAT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(BATS DEFAULT_MSG BATS)

mark_as_advanced(BATS)
