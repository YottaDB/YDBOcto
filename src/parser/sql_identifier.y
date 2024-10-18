/****************************************************************
 *								*
 * Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

sql_identifier
  : sql_identifier_minus_a_few { $$ = $sql_identifier_minus_a_few; }
  | sql_identifier_exceptions {$$ = $sql_identifier_exceptions; }
  | CHECK { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "check"); }
  | PRIMARY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "primary"); }
  | UNIQUE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "unique"); }
  | NULL_TOKEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "null"); }
  ;

sql_identifier_minus_primary_unique_check
  : sql_identifier_minus_a_few { $$ = $sql_identifier_minus_a_few; }
  | sql_identifier_exceptions {$$ = $sql_identifier_exceptions; }
  ;

sql_identifier_exceptions
  : ALL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "all"); }
  | DISTINCT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "distinct"); }
  | UNKNOWN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "unknown"); }
  | FALSE_TOKEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "false"); }
  | TRUE_TOKEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "true"); }
  | CROSS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "cross"); }
  | FULL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "full"); }
  | INNER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "inner"); }
  | JOIN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "join"); }
  | LEFT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "left"); }
  | NATURAL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "natural"); }
  | RIGHT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "right"); }
  | SELECT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "select"); }
  | TABLE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "table"); }
  | CASE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "case"); }
  | NOT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "not"); }
  | AND { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "and"); }
  | BETWEEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "between"); }
  | COLLATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "collate"); }
  | ILIKE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "ilike"); }
  | IN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "in"); }
  | IS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "is"); }
  | LIKE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "like"); }
  | OR { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "or"); }
  | OVER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "over"); }
  | SIMILAR { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "similar"); }
  | WHEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "when"); }
  | AS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "as"); }
  | DEFAULT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "default"); }
  | THEN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "then"); }
  ;

sql_identifier_minus_a_few
  : sql_identifier_minus_more { $$ = $sql_identifier_minus_more; }
  | ANY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "any"); }
  | SOME { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "some"); }
  | AVG { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "avg"); }
  | MAX { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "max"); }
  | MIN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "min"); }
  | SUM { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "sum"); }
  | ARRAY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "array"); }
  | LEAST { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "least"); }
  | CAST { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "cast"); }
  | COALESCE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "coalesce"); }
  | COUNT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "count"); }
  | EXISTS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "exists"); }
  | GREATEST { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "greatest"); }
  | NULLIF { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "nullif"); }
  | DATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "date"); }
  | TIME { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "time"); }
  | TIMESTAMP { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "timestamp"); }
  | VALUES { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "values"); }
  ;

sql_identifier_minus_more
  : ADVANCE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "advance"); }
  | AIMTYPE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "aimtype"); }
  | ASC { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "asc"); }
  | BEG { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "begin"); }
  | BIGINT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "bigint"); }
  | BOOL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "bool"); }
  | BOOLEAN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "boolean"); }
  | BY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "by"); }
  | CASCADE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "cascade"); }
  | CHAR { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "char"); }
  | CHARACTER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "character"); }
  | COMMIT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "commit"); }
  | CONSTRAINT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "constraint"); }
  | CORRESPONDING { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "corresponding"); }
  | CREATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "create"); }
  | DEALLOCATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "deallocate"); }
  | DEC { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "dec"); }
  | DECIMAL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "decimal"); }
  | DELETE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "delete"); }
  | DELIM { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "delim"); }
  | DESC { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "desc"); }
  | DISCARD { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "discard"); }
  | DROP { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "drop"); }
  | ELSE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "else"); }
  | END { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "end"); }
  | ENDPOINT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "endpoint"); }
  | EXCEPT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "except"); }
  | EXTRACT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "extract"); }
  | FROM { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "from"); }
  | FUNCTION { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "function"); }
  | DATE_TIME_FILEMAN { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "fileman"); }
  | GLOBAL { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "global"); }
  | GROUP { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "group"); }
  | HAVING { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "having"); }
  | DATE_TIME_HOROLOG { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "horolog"); }
  | IF { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "if"); }
  | INDEX { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "index"); }
  | INSERT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "insert"); }
  | INT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "int"); }
  | INT2 { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "int2"); }
  | INT4 { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "int4"); }
  | INT8 { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "int8"); }
  | INTEGER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "integer"); }
  | INTERSECT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "intersect"); }
  | INTO { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "into"); }
  | KEEPDATA { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "keepdata"); }
  | KEY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "key"); }
  | LIMIT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "limit"); }
  | NAME { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "name"); }
  | NUM { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "num"); }
  | NUMERIC { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "numeric"); }
  | ON { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "on"); }
  | ORDER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "order"); }
  | OUTER { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "outer"); }
  | PACK { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "pack"); }
  | PARTITION { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "partition"); }
  | PIECE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "piece"); }
  | PREPARE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "prepare"); }
  | READONLY { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "readonly"); }
  | READWRITE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "readwrite"); }
  | REGCLASS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "regclass"); }
  | REGPROC { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "regproc"); }
  | RESTRICT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "restrict"); }
  | RETURNS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "returns"); }
  | ROLLBACK { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "rollback"); }
  | SET { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "set"); }
  | SHOW { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "show"); }
  | SMALLINT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "smallint"); }
  | START { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "start"); }
  | STARTINCLUDE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "startinclude"); }
  | TEXT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "text"); }
  | TO { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "to"); }
  | TRUNCATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "truncate"); }
  | UNION { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "union"); }
  | UNPACK { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "unpack"); }
  | UPDATE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "update"); }
  | USING { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "using"); }
  | VARCHAR { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "varchar"); }
  | VARYING { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "varying"); }
  | VIEW { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "view"); }
  | WHERE { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "where"); }
  | EXIT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "exit"); }
  | QUIT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "quit"); }
  | XREFS { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "xrefs"); }
  | DATE_TIME_ZHOROLOG { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "zhorolog"); }
  | DATE_TIME_ZUT { SQL_VALUE_STATEMENT($$, STRING_LITERAL, "zut"); }
  ;

