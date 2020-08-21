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

-- This is invoked from Stage 2 of the TDT04 subtest

-- TDT04 : OCTO90 :  DROP TABLE should delete db nodes for plans that relied on the dropped table

DROP TABLE orders;

