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

-- TRTE07 : Optimize LIKE (and SIMILAR TO) operator to EQUALS operator if no wildcards are used in pattern

-- Test pattern with no regex or escape sequences
select id from names where 'abcd' like 'abc';

-- Test pattern with no regex but with escape sequences
select id from names where 'abcd' like 'ab\_c';
select id from names where 'abcd' like 'ab\%c';
select id from names where 'abcd' like 'ab\|c';
select id from names where 'abcd' like 'ab\*c';
select id from names where 'abcd' like 'ab\+c';
select id from names where 'abcd' like 'ab\(c';
select id from names where 'abcd' like 'ab\[c';
select id from names where 'abcd' like 'ab\{c';
select id from names where 'abcd' like 'ab\?c';
select id from names where 'abcd' like 'ab\}c';
select id from names where 'abcd' like 'ab\]c';
select id from names where 'abcd' like 'ab\)c';

-- Test pattern with regex but no escape sequences
select id from names where 'abcd' like 'ab_d';
select id from names where 'abcd' like 'ab%d';
select id from names where 'abcd' like 'ab|d';
select id from names where 'abcd' like 'ab*d';
select id from names where 'abcd' like 'ab+d';
select id from names where 'abcd' like 'ab(d';
select id from names where 'abcd' like 'ab[d';
select id from names where 'abcd' like 'ab{d';
select id from names where 'abcd' like 'ab?d';
select id from names where 'abcd' like 'ab}d';
select id from names where 'abcd' like 'ab]d';
select id from names where 'abcd' like 'ab)d';

