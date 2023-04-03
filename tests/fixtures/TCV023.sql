#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
CREATE FUNCTION THREEPARMFUNC(VARCHAR,INTEGER,INTEGER) RETURNS VARCHAR AS $$threeparmfunc^functions;

create view v1 AS select * FROM names WHERE THREEPARMFUNC(PARMLESSFUNC(),2,3) = 'SUCCESS';
select * from v1;
drop view v1;

