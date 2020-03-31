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

-- when same pattern is used in subsequent queries
-- operation type should also be considered to prevent using the wrong
-- format pattern string for processing i.e in the below case
-- pattern string formatted according to SIMILAR TO should not be used
-- for processing ~ query, as the result expectation is different for
-- SIMILAR TO and ~
select id from names where 'ab' similar to 'a';
select id from names where 'ab' ~ 'a';
select id from names where 'abc' ~ 'a';
