#################################################################
#								#
# Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT017 : OCTO636 : SIZE specified in the VARCHAR type of the typecast operator (::) should be honored

-- Below queries have no UTF-8 data

SELECT '|' || 'abcd' || '|';
SELECT '|' || 'abcd'::varchar(2) || '|';
SELECT '|' || 'abcd'::varchar(3) || '|';
SELECT '|' || 'abcd'::varchar(4) || '|';
SELECT '|' || 'abcd'::varchar(5) || '|';

-- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/636#note_1057436057
select col1 is NULL from (select NULL::varchar(2) as col1) n1;

