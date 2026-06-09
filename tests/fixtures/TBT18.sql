#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE tbt18 (product_id INT PRIMARY KEY, name VARCHAR, available BOOLEAN) GLOBAL "^tbt18" DELIM "|" READONLY;
-- Referencing the BOOLEAN column forces an AIM xref build on `available`. Before OCTO1131, ForceBoolean
-- returned $ZYSQLNULL for any value it did not recognize as a boolean, and AIM died with ZYSQLNULLNOTVALID
-- at xrefdata^%YDBAIM when it used that as a global variable subscript. After the fix the xref builds cleanly:
--   * an empty piece (^tbt18(300)="gizmo|") maps to the empty string, i.e. SQL NULL
--   * any other unrecognized value (^tbt18(600)="doohickey|not a boolean") maps to 0, i.e. FALSE
-- The NULL row appears in neither result below; the unrecognized-value row appears only under `= FALSE`.
SELECT * FROM tbt18 WHERE available = TRUE;
SELECT * FROM tbt18 WHERE available = FALSE;
