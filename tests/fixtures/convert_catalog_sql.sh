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
cp octo-seed.sql $1/octo-seed.sql
 sed -n '/CREATE TABLE pg_catalog/,/^$/p' octo-seed.sql |	# Extract SQL statements only, no comments
	sed 's/\(CREATE TABLE \)pg_catalog\.\(.*\)/\1\2/' >> $1/octo-seed.sql # Remove prefixes

 sed -n '/CREATE FUNCTION PG_CATALOG/,/^$/p' octo-seed.sql |			# Extract SQL statements only, no comments
	sed 's/\(CREATE FUNCTION \)PG_CATALOG\.\(.*\)/\1\2/' >> $1/octo-seed.sql 	# Remove prefixes
