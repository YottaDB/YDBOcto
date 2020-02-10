#!/bin/bash
#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Copy CREATE TABLE statements for PostgreSQL catalog tables without "pg_catalog" or "information_schema" prefixes
cp postgres-seed.sql $1/postgres-seed.sql
 sed -n '/CREATE TABLE pg_catalog/,/^$/p' postgres-seed.sql |	# Extract SQL statements only, no comments
	sed 's/\(CREATE TABLE \)pg_catalog\.\(.*\)/\1\2/' | 	# Remove prefixes
	sed 's/""pg_catalog"",//' >> $1/postgres-seed.sql	# Remove pg_catalog subscript from GLOBAL
