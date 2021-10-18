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
# OCTO761: Verify that PIECE keyword is ignored if EXTRACT is specified
# https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1012#note_707718044
CREATE TABLE tmp (id INTEGER PRIMARY KEY, datetime VARCHAR PIECE 2 EXTRACT "$HOROLOG");
SELECT * FROM tmp;
