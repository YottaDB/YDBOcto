#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE largecolnames(a1234567890 int, b1234567890 int, c1234567890 int,
	PRIMARY KEY (a1234567890, b1234567890, c1234567890)
)
GLOBAL "^LARGECOLNAMES";

