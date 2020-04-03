#################################################################
#                                                               #
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################
-- Tests law of the excluded fourth (in WHERE clauses)
-- -- For more information refer https://en.wikipedia.org/wiki/Null_(SQL)#Law_of_the_excluded_fourth_(in_WHERE_clauses)
-- -- should not have the null row
SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)) n1 WHERE (id = 0) OR NOT (id = 0) ORDER BY n1.id;
-- -- should have the null row
 SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)) n1 WHERE (id = 0) OR NOT (id = 0) OR (id IS NULL) ORDER BY n1.id;
-- -- should have the null row
SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)) n1 ORDER BY n1.id;
