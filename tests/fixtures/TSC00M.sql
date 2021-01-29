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

-- TSC00M : select some columns with C-style and #-style comments interwoven

SELECT firstName /* We need first names */
  , lastName /* We also need last names */
  /* It might also be helpful to get id
   * but mostly because it lets me test multiline
   * comments */
  , id
# We're also going to try a different kind of comment here
FROM names;

select 1 as QUERY2;
/*
 */
select 2 as QUERY3;

