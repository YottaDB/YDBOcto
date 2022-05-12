#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# AIM#60 VistA test case (non-Fileman compliant data)
select count(*) from `SORT_TEMP_S_F_D_R_J_F_DATA`;
select count(*) from `SORT_TEMP_S_F_D_R_J_F_DATA` where `RELATIONAL_START_FILE_NO` is NULL;
select count(*) from `SORT_TEMP_S_F_D_R_J_F_DATA` where `RELATIONAL_START_FILE_NO` is not NULL;
