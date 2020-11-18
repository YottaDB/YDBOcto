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

-- TC038 : OCTO626 : CREATE TABLE should issue ERR_DUPLICATE_COLUMN error if more than one column with the same name is specified

CREATE TABLE tmp (id INTEGER, id INTEGER);
CREATE TABLE tmp (firstname VARCHAR, firstname VARCHAR);
CREATE TABLE tmp (id INTEGER, firstname VARCHAR, id INTEGER);
CREATE TABLE tmp (id INTEGER, id INTEGER, firstname VARCHAR);
CREATE TABLE tmp (firstname VARCHAR, id INTEGER, firstname VARCHAR);
CREATE TABLE tmp (firstname VARCHAR, firstname VARCHAR, id INTEGER);
CREATE TABLE tmp (id INTEGER, firstname VARCHAR, lastname VARCHAR, firstname INTEGER, id2 INTEGER);

