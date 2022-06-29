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

-- TERR041 : OCTO738 : Test ydb_error_check assert succeeds when M code calls ZHALT
drop function if exists XECUTE_M_CODE(VARCHAR);
create function XECUTE_M_CODE(VARCHAR) returns integer as $$octo738^octo738;
select XECUTE_M_CODE("zhalt 1");
