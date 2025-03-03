#################################################################
#								#
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TDTT111: OCTO382 : Verify that timestamp -> zhorolog conversion for microseconds happens correctly
SELECT timestamp'2969-3-18 16:31:45.365000';
SELECT timestamp'2969-3-18 16:31:45.365';
