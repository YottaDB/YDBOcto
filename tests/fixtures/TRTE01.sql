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

-- TRTE01 : LIKE/SIMILAR TO etc. usages in names database

-- LIKE variants
select id from names where firstname like 'Z%';
select id from names where 'abc' like 'abc';
select id from names where 'abc' like 'ABC';
select id from names where 'abc' like 'xyz';
select id from names where 'abc' like 'a';
select id from names where 'abc' like '%';
select id from names where '%' like '\%';
select id from names where 'abc' like '___';
select id from names where 'abc' like 'a_c';
select id from names where 'abc' like '.*';
select id from names where '.*' like '.*';
select id from names where '.*' like '\.\*';
select id from names where '\.\*' like '\\.\\*';
select id from names where 'aaaa' like 'a*';
select id from names where 'aaaa' like 'a\*';
select id from names where 'a' like '[abc]';
select id from names where '[abc]' like '[abc]';
select id from names where '(abc)' like '\(abc\)';
select id from names where 'a{4}' like 'a\{4\}';
select id from names where 'a?' like 'a\?';
select id from names where 'a+' like 'a\+';
select id from names where 'abc|d' like 'abc\|d';
select id from names where '%' like '\%';
select id from names where 'abc' like 'abc|d';
select id from names where 'abc' like '(xyz)|(abc)';
select id from names where 'a' like 'ab?';
select id from names where 'aaaa' like 'a+';

-- NOT LIKE variants
select id from names where firstname not like 'Z%';
select id from names where 'abc' not like 'abc';
select id from names where 'abc' not like 'ABC';
select id from names where 'abc' not like 'xyz';
select id from names where 'abc' not like 'a';
select id from names where 'abc' not like '%';
select id from names where 'abc' not like '___';
select id from names where 'abc' not like 'a_c';
select id from names where 'abc' not like '.*';
select id from names where '.*' not like '.*';
select id from names where '.*' not like '\.\*';
select id from names where '\.\*' not like '\\.\\*';
select id from names where 'aaaa' not like 'a*';
select id from names where 'aaaa' not like 'a\*';
select id from names where 'a' not like '[abc]';
select id from names where '[abc]' not like '[abc]';
select id from names where '(abc)' not like '\(abc\)';
select id from names where 'a{4}' not like 'a\{4\}';
select id from names where 'a?' not like 'a\?';
select id from names where 'a+' not like 'a\+';
select id from names where 'abc|d' not like 'abc\|d';
select id from names where '%' not like '\%';
select id from names where 'abc' not like 'abc|d';

-- ILIKE variants
select id from names where 'abc' ilike 'abc';
select id from names where 'abc' ilike 'ABC';
select id from names where 'abc' ilike 'AbC';
select id from names where 'ABC' ilike 'abc';

-- NOT ILIKE variants
select id from names where 'xyz' not ilike 'abc';
select id from names where 'xyz' not ilike 'XYZ';
select id from names where 'xyz' not ilike 'ayz';

-- ~~ variants
select id from names where 'abc' ~~ 'abc';
select id from names where 'abc' ~~ 'ABC';
select id from names where 'abc' ~~ 'xyz';
select id from names where 'abc' ~~ 'a';
select id from names where 'abc' ~~ '%';
select id from names where 'abc' ~~ '___';
select id from names where 'abc' ~~ 'a_c';
select id from names where 'abc' ~~ '.*';
select id from names where '.*' ~~ '.*';
select id from names where '.*' ~~ '\.\*';
select id from names where '\.\*' ~~ '\\.\\*';
select id from names where 'aaaa' ~~ 'a*';
select id from names where 'aaaa' ~~ 'a\*';
select id from names where 'a' ~~ '[abc]';
select id from names where '[abc]' ~~ '[abc]';

-- !~~ variants
select id from names where 'abc' !~~ 'abc';
select id from names where 'abc' !~~ 'ABC';
select id from names where 'abc' !~~ 'xyz';
select id from names where 'abc' !~~ 'a';
select id from names where 'abc' !~~ '%';
select id from names where 'abc' !~~ '___';
select id from names where 'abc' !~~ 'a_c';
select id from names where 'abc' !~~ '.*';
select id from names where '.*' !~~ '.*';
select id from names where '.*' !~~ '\.\*';
select id from names where '\.\*' !~~ '\\.\\*';
select id from names where 'aaaa' !~~ 'a*';
select id from names where 'aaaa' !~~ 'a\*';
select id from names where 'a' !~~ '[abc]';
select id from names where '[abc]' !~~ '[abc]';

-- ~~* variants
select id from names where 'abc' ~~* 'abc';
select id from names where 'abc' ~~* 'ABC';
select id from names where 'abc' ~~* 'AbC';
select id from names where 'ABC' ~~* 'abc';

-- !~~* variants
select id from names where 'xyz' !~~* 'abc';
select id from names where 'xyz' !~~* 'XYZ';
select id from names where 'xyz' !~~* 'ayz';

-- SIMILAR TO variants
select id from names where 'abc' similar to 'abc';
select id from names where 'abc' similar to 'ABC';
select id from names where 'abc' similar to 'xyz';
select id from names where 'abc' similar to 'a';
select id from names where 'abc' similar to '%';
select id from names where '%' similar to '\%';
select id from names where 'abc' similar to '___';
select id from names where 'abc' similar to '_*';
select id from names where 'abc' similar to '_\*';
select id from names where 'a*' similar to '_\*';
select id from names where '_' similar to '\_';
select id from names where '...' similar to '...';
select id from names where '.*' similar to '\.\*';
select id from names where 'aaaa' similar to 'a{4}';
select id from names where 'aaaa' similar to 'a{1,}';
select id from names where 'aaaa' similar to 'a{1,10}';
select id from names where 'a' similar to '[abc]';
select id from names where 'abc' similar to 'abc|d';
select id from names where 'abc|d' similar to 'abc\|d';
select id from names where 'abc' similar to '(xyz)|(abc)';
select id from names where '(abc)' similar to '\(abc\)';
select id from names where 'aaaa' similar to 'a+';
select id from names where 'a+' similar to 'a\+';
select id from names where 'a' similar to 'ab?';
select id from names where 'a?' similar to 'a\?';
select id from names where 'ab' similar to '.*';

-- NOT SIMILAR TO variants
select id from names where 'abc' not similar to 'abc';
select id from names where 'abc' not similar to 'ABC';
select id from names where 'abc' not similar to 'xyz';
select id from names where 'abc' not similar to 'a';
select id from names where 'abc' not similar to '%';
select id from names where '%' not similar to '\%';
select id from names where 'abc' not similar to '___';
select id from names where 'abc' not similar to '_*';
select id from names where 'abc' not similar to '_\*';
select id from names where 'a*' not similar to '_\*';
select id from names where '_' not similar to '\_';
select id from names where '...' not similar to '...';
select id from names where '.*' not similar to '\.\*';
select id from names where 'aaaa' not similar to 'a{4}';
select id from names where 'aaaa' not similar to 'a{1,}';
select id from names where 'aaaa' not similar to 'a{1,10}';
select id from names where 'a' not similar to '[abc]';
select id from names where 'abc' not similar to 'abc|d';
select id from names where 'abc|d' not similar to 'abc\|d';
select id from names where 'abc' not similar to '(xyz)|(abc)';
select id from names where '(abc)' not similar to '\(abc\)';
select id from names where 'aaaa' not similar to 'a+';
select id from names where 'a+' not similar to 'a\+';
select id from names where 'a' not similar to 'ab?';
select id from names where 'a?' not similar to 'a\?';

-- ~ variants
select id from names where 'abc' ~ 'abc';
select id from names where 'abc' ~ 'ABC';
select id from names where 'abc' ~ 'xyz';
select id from names where 'abc' ~ 'a';
select id from names where 'abc' ~ 'a.*';
select id from names where 'abc' ~ '...';
select id from names where 'abc' ~ '___';
select id from names where '___' ~ '___';
select id from names where 'abc' ~ '%';
select id from names where '%' ~ '%';
select id from names where 'aaaa' ~ 'a*';
select id from names where '.*' ~ '\.\*';
select id from names where 'a' ~ '[abc]';
select id from names where 'abc|d' ~ 'abc|d';
select id from names where 'a+' ~ 'a+';
select id from names where 'a?' ~ 'a?';
select id from names where '(abc)' ~ '\(abc\)';
select id from names where 'a?' ~ 'a\?';
select id from names where 'a+' !~ 'a\+';
select id from names where 'abc|d' ~ 'abc\|d';
select id from names where 'abc' ~ '(xyz)|(abc)';
select id from names where 'a' ~ 'ab?';
select id from names where 'aaaa' ~ 'a+';

-- !~ variants
select id from names where 'abc' !~ 'abc';
select id from names where 'abc' !~ 'a';
select id from names where 'abc' !~ 'a.*';
select id from names where 'abc' !~ '...';
select id from names where 'abc' !~ '___';
select id from names where '___' !~ '___';
select id from names where 'abc' !~ '%';
select id from names where '%' !~ '%';
select id from names where 'aaaa' !~ 'a*';
select id from names where '.*' !~ '\.\*';
select id from names where 'a' !~ '[abc]';
select id from names where 'abc|d' !~ 'abc|d';
select id from names where 'a+' !~ 'a+';
select id from names where 'a?' !~ 'a?';
select id from names where '(abc)' !~ '\(abc\)';
select id from names where 'a?' !~ 'a\?';
select id from names where 'a+' !~ 'a\+';
select id from names where 'abc|d' !~ 'abc\|d';

-- ~* variants
select id from names where 'abc' ~* 'abc';
select id from names where 'abc' ~* 'ABC';
select id from names where 'abc' ~* 'AbC';
select id from names where 'abc' ~* 'A';
select id from names where 'aAaAaAa' ~* 'a*';

-- !~* variants
select id from names where 'xyz' !~* 'abc';
select id from names where 'xyz' !~* 'XYZ';
select id from names where 'xyz' !~* 'ayz';
select id from names where 'xyz' !~* 'a';
select id from names where 'xyz' !~* 'x';
select id from names where 'xyz' !~* 'X';

-- Variants using |{}()+? in combination with backslash
select id from names where 'abd' similar to 'abc|d';
select id from names where 'abd' not similar to 'abc|d';
select id from names where 'aaaa' ~ 'a\{4\}';
select id from names where 'aaaa' ~ 'a\{1,\}';
select id from names where 'aaaa' ~ 'a\{1,10\}';
select id from names where 'abc' ~ 'abc\|d';
select id from names where 'abd' ~ 'abc\|d';
select id from names where 'abc' ~ '\(xyz\)\|\(abc\)';
select id from names where 'aaaa' ~ 'a\+';
select id from names where 'a' ~ 'ab\?';
select id from names where 'aaaa' !~ 'a\{4\}';
select id from names where 'aaaa' !~ 'a\{1,\}';
select id from names where 'aaaa' !~ 'a\{1,10\}';
select id from names where 'abc' !~ 'abc\|d';
select id from names where 'abd' !~ 'abc\|d';
select id from names where 'abc' !~ '\(xyz\)\|\(abc\)';
select id from names where 'aaaa' !~ 'a\+';
select id from names where 'a' !~ 'ab\?';
select id from names where 'aaaa' !~ 'a{1,10}';
select id from names where 'aaaa' !~ 'a{1,}';
select id from names where 'aaaa' !~ 'a{4}';
select id from names where 'aaaa' ~ 'a{1,10}';
select id from names where 'aaaa' ~ 'a{1,}';
select id from names where 'aaaa' ~ 'a{4}';

-- Variants using backslash inside single-quotes. Moved to end as they otherwise disturb syntax highlighting in rest of sections
select id from names where '\\' like '\\';
select id from names where '\\' not like '\\';
select id from names where '\\' ~~ '\\';
select id from names where '\\' !~~ '\\';
select id from names where '\' like '\\';
select id from names where '\' not like '\\';
select id from names where '\' ~~ '\\';
select id from names where '\' !~~ '\\';
select id from names where '\' similar to '\\';
select id from names where '\' not similar to '\\';
select id from names where '\' ~ '\\';
select id from names where '\' !~ '\\';

-- Verifies queries where octo performs a trim on .* values
select * from names where (id*3)::varchar like '%1%';
select * from names where firstname ~ '.*a';
select * from names where firstname like '%a';
select * from names where firstname like '%';
select * from names where firstname like '%%%%%%';
select * from names where firstname not like '%';
select * from names where firstname not like '%%%%%%';
select * from names where firstname similar to '%a';
select * from names where firstname similar to '%';
select * from names where firstname similar to '%%%%%%';
select * from names where firstname not similar to '%';
select * from names where firstname not similar to '%%%%%%';
select * from names where firstname ~ '.*';
select * from names where firstname ~ '.*.*.*.*';
select * from names where firstname !~ '.*';
select * from names where firstname !~ '.*.*.*.*';
select * from names where firstname ~* '.*';
select * from names where firstname ~* '.*.*.*.*';
select * from names where firstname !~* '.*';
select * from names where firstname !~* '.*.*.*.*';
select * from names where firstname like '%%abc%%';
select * from names where firstname not like '%%abc%%';
select * from names where firstname similar to '%%abc%%';
select * from names where firstname not similar to '%%abc%%';
