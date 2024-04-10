#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE FUNCTION absf(INTEGER) RETURNS INTEGER AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION concatf(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
create view v1 as select CONCATF(firstname, ABSF(id-10)::varchar) from names;
drop function concatf(varchar,varchar);
drop function absf(integer);
