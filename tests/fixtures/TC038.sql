#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC038 : OCTO626 : CREATE TABLE should issue ERR_DUPLICATE_COLUMN error if more than one column with the same name is specified

CREATE TABLE tmp (id INTEGER, id INTEGER);
CREATE TABLE tmp (firstname VARCHAR, firstname VARCHAR);
CREATE TABLE tmp (id INTEGER, firstname VARCHAR, id INTEGER);
CREATE TABLE tmp (id INTEGER, id INTEGER, firstname VARCHAR);
CREATE TABLE tmp (firstname VARCHAR, id INTEGER, firstname VARCHAR);
CREATE TABLE tmp (firstname VARCHAR, firstname VARCHAR, id INTEGER);
CREATE TABLE tmp (id INTEGER, firstname VARCHAR, lastname VARCHAR, firstname INTEGER, id2 INTEGER);

-- Test that %yo_keycol (the name of the hidden column) is not a user-defined column name that can be used in a table
-- if the table had the hidden key column implicitly inserted (which it would if no primary key columns are specified
-- and READWRITE is specified).
CREATE TABLE tmp (firstName VARCHAR, lastName VARCHAR, `%yo_keycol` INTEGER) READWRITE;

