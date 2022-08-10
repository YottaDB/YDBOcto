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

-- TCT018 : OCTO636 : SIZE specified in the NUMERIC type of the typecast operator (::) should be honored

-- Note: The SIZE field for the INTEGER type is currently accepted due to a user request but ignored by Octo.
--       Therefore, there are no tests like the below for the INTEGER type.

-- Below queries are valid (i.e. produce no errors)

SELECT 1.50::NUMERIC(2);
SELECT 1.49::NUMERIC(3);
SELECT 1.49::NUMERIC(2);
SELECT 1.49::NUMERIC(1);
SELECT 15.54::NUMERIC(4);
SELECT 15.54::NUMERIC(3);
SELECT 15.54::NUMERIC(2);
SELECT -1.50::NUMERIC(2);
SELECT -1.49::NUMERIC(3);
SELECT -1.49::NUMERIC(2);
SELECT -1.49::NUMERIC(1);
SELECT -15.54::NUMERIC(4);
SELECT -15.54::NUMERIC(3);
SELECT -15.54::NUMERIC(2);
SELECT 1.50::NUMERIC(2,0);
SELECT 1.49::NUMERIC(2,0);
SELECT 1.49::NUMERIC(2,1);
SELECT 1.49::NUMERIC(3,1);
SELECT 1.49::NUMERIC(3,2);
SELECT 15.54::NUMERIC(4,1);
SELECT 15.54::NUMERIC(3,0);
SELECT 15.54::NUMERIC(3,1);
SELECT -1.50::NUMERIC(2,0);
SELECT -1.49::NUMERIC(2,0);
SELECT -1.49::NUMERIC(2,1);
SELECT -1.49::NUMERIC(3,1);
SELECT -1.49::NUMERIC(3,2);
SELECT -15.54::NUMERIC(4,1);
SELECT -15.54::NUMERIC(3,0);
SELECT -15.54::NUMERIC(3,1);

-- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/636#note_1057433821
SELECT NULL::NUMERIC(10,2);

