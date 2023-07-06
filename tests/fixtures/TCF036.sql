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

-- TCF036 : OCTO519 : Functions with double-quoted identifiers

select "ABS"(-1);
select "abs"(-1); -- Error: function name was defined as uppercase
select "concat"(firstname, ' ', lastname) from names;
select "CONCAT"(firstname, ' ', lastname) from names; -- Error: function name was defined as lowercase

CREATE FUNCTION "concat"(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
CREATE FUNCTION "abs"(INTEGER) RETURNS INTEGER AS $$ABS^%ydboctosqlfunctions;
select "abs"(-1);
select "concat"(firstname, ' ', lastname) from names;
