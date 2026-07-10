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
-- TC088 Section B : YDBOcto#763/#764 : whole-node and DELIMS/PIECES SUBSTR over multi-byte data
--
-- Section A covered PIECE + SUBSTR. The other two extraction forms must count characters the
-- same way, so both are repeated here on the same "mixed"/"cjk"/"ascii" values.

-- ^u2(id,0) is the whole node to extract from; ^u2(id,1) names the case.
-- A whole-node SUBSTR column must name its own column-level GLOBAL.
CREATE TABLE u2 (
	id	INTEGER PRIMARY KEY,
	label	VARCHAR GLOBAL "^u2(keys(""id""),1)" PIECE 1,
	head3	VARCHAR SUBSTR 1-3 GLOBAL "^u2(keys(""id""),0)",
	char4	VARCHAR SUBSTR 4-4 GLOBAL "^u2(keys(""id""),0)"
) GLOBAL "^u2(keys(""id""))" READONLY;

select label, head3, char4 from u2 order by label;
select label from u2 where head3 = 'Xé日';

-- (The stored-text round-trip of the whole-node form is not repeated here; it is not
-- chset-specific and TC087 Section A already covers it.)

-- ^u3 is "<junk>|<value>^<junk>|<label>", so the value is reached two delimiter levels deep
-- and the character range applies to the innermost piece:
--   DELIMS ("|","^") PIECES (2,1) SUBSTR 1-3 == $EXTRACT($PIECE($PIECE(node,"|",2),"^",1),1,3)
CREATE TABLE u3 (
	id	INTEGER PRIMARY KEY,
	label	VARCHAR PIECE 3,
	head3	VARCHAR DELIMS ("|","^") PIECES (2,1) SUBSTR 1-3,
	char4	VARCHAR DELIMS ("|","^") PIECES (2,1) SUBSTR 4-4
) GLOBAL "^u3(keys(""id""))" READONLY;

select label, head3, char4 from u3 order by label;
select label from u3 where head3 = 'Xé日';

-- Every generated $EXTRACT, to confirm the character range is applied to the piece result
-- rather than to the whole node.
