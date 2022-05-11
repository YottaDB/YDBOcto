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

-- TERR037 : OCTO600 : Test query string abbreviation in syntax errors

SELECT badcolumn, notsogoodcolumn, maybeokcolumn from nonexistenttable;
SELECT firstname, lastname, middlename from names as extralongnamealias;
SELECT
	firstname,
	multilinequery,
	lastname
from names;
SELECT
	multi,
	line,
	query
from nonexistenttable;
