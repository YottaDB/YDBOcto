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

-- TW29 Case 4 (YDBOcto#1124) : the column's global uses a DIFFERENT global name than the table's rows, and
-- lists ALL primary-key columns in primary-key order. The rows live in ^SEAT(event_id,seat_id); "holder" is
-- stored in a separate global ^HOLDER(event_id,seat_id). Every primary-key subscript is in the cross
-- reference, so all keys are advanced from it (the cross reference is built on ^HOLDER, not ^SEAT); no key
-- is advanced from the table's own global.
CREATE TABLE seats (
 event_id INTEGER,
 seat_id INTEGER,
 holder VARCHAR(20) GLOBAL "^HOLDER(keys(""event_id""),keys(""seat_id""))" PIECE 1,
 price VARCHAR(10) PIECE 1,
 PRIMARY KEY (event_id, seat_id)
) GLOBAL "^SEAT" READONLY;

-- Unfiltered : confirms the data is present.
SELECT * FROM seats ORDER BY event_id, seat_id;

-- Filtered : WHERE on the column stored in a different global (all keys present, in order).
SELECT * FROM seats WHERE holder = 'Alice' ORDER BY event_id, seat_id;
