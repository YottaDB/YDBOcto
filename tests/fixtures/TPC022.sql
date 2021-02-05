#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TPC022 : OCTO683 : Test that SHOW command sends RowDescription messages with Simple Query Protocol (psql client)

-- Below are the queries sent by Excel using Simple Query Protocol at connection startup.
-- Hence including the SET queries too even though the main objective of this test is to test the SHOW command.

SET DateStyle = 'ISO';
SET extra_float_digits = 2;
SHOW transaction_isolation;
SHOW DateStyle;
SHOW extra_float_digits;
SHOW max_identifier_length;

