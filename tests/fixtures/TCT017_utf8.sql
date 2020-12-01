#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT017 : OCTO636 : SIZE specified in the VARCHAR type of the typecast operator (::) should be honored

-- Below queries have UTF-8 data

SELECT '|' || 'ＡＢＣＤ' || '|';
SELECT '|' || 'ＡＢＣＤ'::varchar(2) || '|';
SELECT '|' || 'ＡＢＣＤ'::varchar(3) || '|';
SELECT '|' || 'ＡＢＣＤ'::varchar(4) || '|';
SELECT '|' || 'ＡＢＣＤ'::varchar(5) || '|';

