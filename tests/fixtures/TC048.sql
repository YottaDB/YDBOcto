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

-- TC048 : OCTO595 : Creation of multiple tables of the same name in sequence with IF NOT EXISTS is skipped with a warning

CREATE TABLE IF NOT EXISTS Customers (CustomerID INTEGER PRIMARY KEY);
CREATE TABLE IF NOT EXISTS Customers (CustomerID INTEGER PRIMARY KEY);
