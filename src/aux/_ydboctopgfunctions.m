;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pgArrayLower(array,dimension) ; TODO this is just a placeholder
	QUIT 0

pgArrayUpper(array,dimension) ; TODO this is just a placeholder
	QUIT 0

pgBackendPid()
	QUIT ""

pgCurrentCatalog() ; TODO this is just a placeholder
	QUIT "octo"

pgCurrentDatabase() ; TODO this is just a placeholder
	QUIT "octo"

pgCurrentSchema() ; TODO this is just a placeholder
	; Until schemas (YDBOcto#99 and/or YDBOcto#417) are supported, "public" is the current schema.
	QUIT "public"

pgCurrentSchemas(implicit) ; TODO this is just a placeholder
	QUIT ""

pgGenerateSeries(start,stop) ; TODO this is just a placeholder
	QUIT 0

pgGetUserById(a) ; TODO this is just a placeholder
	; Returns a role's name given its OID.
	; Octo currently doesn't distinguish between roles so return "octo" as it is the only role present
        QUIT "octo"

pgIsInRecovery() ; TODO this is just a placeholder
	; This function returns true if the database is in the process of recovering from a failure by restoring a backup.
	; Since Octo doesn't support this feature presently, just return false (0).
	QUIT 0

pgIsXlogReplayPaused() ; TODO this is just a placeholder
	; This function returns true if the database has paused the process of recovering from a failure by restoring a backup.
	; Since Octo doesn't support this feature presently, just return false (0).
	QUIT 0

pgUser()
	; Look up the name of the currently active user. Note that this function currently implements three SQL functions:
	; current_user, current_role, and session_user. This is because Octo presently does not distinguish between roles, users,
	; and session users. If these distinctions are ever implemented, then this function will need to be broken up into several
	; different functions accordingly.
	;
	; Similarly, note also that currently no session_id is needed to look up the username for the current session. This is because the
	; session_id will always be the same, as described in rocto.c where the session_id is created by ydb_incr_s. However, if
	; sessions are distinguished in the future and, accordingly, the session_id may vary, then this code will need to be updated
	; to include the session_id as a subscript for the lookup of the username.
	QUIT $GET(%ydboctosession("user"))

pgEncodingToChar(encnum)
	; Convert encoding from INTEGER representation to VARCHAR.
	; Until pg_database is fully implemented (#417), always return SQL_ASCII.
	QUIT "SQL_ASCII"

pgGetExpr(a,b,c,d,e,f) ; TODO this is just a placeholder
	QUIT ""

pgTotalRelationSize(tableoid) ; TODO this is just a placeholder
	QUIT ""

pgRelationSize(tableoid) ; TODO this is just a placeholder
	QUIT ""

pgHasDatabasePrivilege(user,database,privilege)
	; Since Octo currently does not implement privileges, always return TRUE.
	; Note that there is a two-argument version of this function as well. If Octo ever implements privileges,
	; logic for that version must be implemented as well.
	QUIT 1

pgObjDescription(a,b)
	QUIT "No description"

pgRowNumber()
	QUIT $I(%ydboctopgRowNumber)

pgTableIsVisible(a) ; TODO this is just a placeholder
	; This function returns true i.e 1 if a certain object
	; is visible in the current schema search path.
	; For example, a table is said to be visible if its containing
	; schema is in the search path and no table of the same name appears earlier in the search path.
        QUIT 1

setConfig(setting,value,local)
	SET %ydboctosession(0,"variables",setting)=value
	QUIT %ydboctosession(0,"variables",setting)

pgRegClass(tablename)
	; -------------------------------------------------------------------------------------------------------------
	; This is a simplistic implementation of "::regclass" that enabled SquirrelSQL to work with JDBC driver 42.6.0.
	; The full fledged implementation will have to wait for YDBOcto#967.
	; -------------------------------------------------------------------------------------------------------------
	QUIT:$ZYISSQLNULL(tablename) $ZYSQLNULL
	; Given a "tablename" argument, we need to find its corresponding OID.
	NEW tbl,tbl2,oid
	; Note that a tablename could be of the form "tablename" or "schemaname"."tablename".
	; Check for both forms in the order "schemaname"."tablename" and then just "tablename" by stripping off
	; the schema name (if present) and retrying the search.  This is similar to what "src/find_table.c" does.
	SET tbl=tablename
	FOR  DO  QUIT:(tbl="")!(oid'=0)  SET tbl=$piece(tbl,".",2)
	. set tbl2=$translate(tbl,"""")	; In case schema or table name is surrounded by double-quotes, remove it before checking.
	. SET oid=$get(^%ydboctoschema(tbl2,"pg_class"),0)
	DO:(0=oid)
	.	SET %ydboctoerror("UNKNOWNTABLE",1)=tablename	; pass parameter to `src/ydb_error_check.c`
	.	ZMESSAGE %ydboctoerror("UNKNOWNTABLE")
	QUIT oid

pgRegProc(procname)
	; -------------------------------------------------------------------------------------------------------------
	; This is a simplistic implementation of "::regproc" that enabled SquirrelSQL to work with JDBC driver 42.6.0.
	; The full fledged implementation will have to wait for YDBOcto#967.
	; -------------------------------------------------------------------------------------------------------------
	; For now we return the procedure/function name as is. This simplistic approach is good enough for right now.
	; Will need to revisit it if/when the need arises.
	QUIT procname

pgFormatType(typeoid,typemod)
	; This implements the "format_type(typeoid,typemod)" function.
	; This function returns the human-readable name of a data type whose oid is "typeoid".
	; For example, "format_type(23,NULL)" would return the string "integer" as the oid of 23 is for the INTEGER type.
	;    The typemod parameter is not applicable for the integer type and so a NULL needs to be passed in for that.
	;    Any non-NULL typemod parameter in that case is ignored by this function.
	; For example, "format_type(1043,34)" would return the string "varchar(30)" as the oid of 1043 is for the VARCHAR type
	;    and the type modified value of "34" implies the VARCHAR is of size 34 - 4 bytes (overhead) = 30.
	; If no data type matches the specified typeoid/typemod combination. the string "???" is returned.
	; See https://pgpedia.info/f/format_type.html for more details
	NEW typename
	; See "PSQL_TypeOid" in "src/octo_types.h" for a list of type oids that Octo currently supports. Those are the
	; ones which are recognized below.
	QUIT:$ZYISSQLNULL(typeoid) "null"	; typemod is ignored in this case
	QUIT:typeoid=16 "boolean"		; typemod is ignored in this case
	QUIT:typeoid=23 "integer"		; typemod is ignored in this case
	IF typeoid=1043 DO  QUIT typename
	.	SET typename="varchar"
	.	; See code in "src/store_table_in_pg_class.c" under "case STRING_TYPE" for how "atttypmod" is updated.
	.	; The below code does the inverse of that logic to get the user specified size.
	.	SET:typemod>4 typename=typename_"("_(typemod-4)_")"
	IF typeoid=1700 DO  QUIT typename
	.	SET typename="numeric"
	.	QUIT:$ZYISSQLNULL(typemod)
	.	; See code in "src/store_table_in_pg_class.c" under "case NUMERIC_TYPE" for how "atttypmod" is updated.
	.	; The below code does the inverse of that logic to get the user specified size.
	.	QUIT:typemod<4		; typemod less than 4 implies an out-of-range value. Handle it by ignoring it.
	.				; Postgres 14 accepts values in the range 0-4 but I suspect that is not intentional.
	.	SET typemod=typemod-4
	.	SET typename=typename_"("_(typemod\(2**16))_","_(typemod#(2**16))_")"
	QUIT "unknown"

pgGetPartKeyDef(tableoid)	;
	; This implements the "pg_catalog.pg_get_partkeydef(tableoid)" function.
	; This is a system function returning the definition of a partition key
	; See https://pgpedia.info/p/pg_get_partkeydef.html for more details
	;
	; Since Octo currently does not support partitioning of tables, this function currently returns NULL always.
	QUIT $ZYSQLNULL

pgGetConstraintDef(constraintoid,pretty)	; TODO this is just a placeholder
	; This implements the "pg_catalog.pg_get_constraintdef()" function.
	; See https://pgpedia.info/p/pg_get_constraintdef.html for more details.
	; Octo currently returns the empty string for the constraint definition.
	QUIT ""

pgSubString(str,start,len)	;
	; This implements the Postgres "substring" function.
	; See https://www.postgresql.org/docs/9.1/functions-string.html for more details.
	; Returns substring starting from "start" up to a length of "len".
	; If "len" is omitted, returns the string from "start" to end.
	; If "start" is omitted, returns the entire string.
	QUIT:$ZYISSQLNULL(str) $ZYSQLNULL
	QUIT:$ZYISSQLNULL(start) $ZYSQLNULL
	QUIT:$ZYISSQLNULL(len) $ZYSQLNULL
	QUIT:'$data(start) str
	IF '$data(len) DO  QUIT $EXTRACT(str,start,$LENGTH(str))
	. IF (0>=start) SET start=1
	ZMESSAGE:(0>len) %ydboctoerror("NEGATIVESUBSTRINGLENGTH")
	IF (0>=start) SET len=start+len-1,start=1
	ELSE  SET len=start+len-1
	QUIT $EXTRACT(str,start,len)

pgStatGetNumScans(tableoid)	;
	; This implements the "pg_catalog.pg_stat_get_numscans()" function.
	; See https://www.postgresql.org/docs/9.1/monitoring-stats.html for more details.
	; Since Octo currently does not currently support statistics collection, this function currently returns 0 always.
	QUIT 0

pgTableSpaceLocation(tablespaceoid)	;
	; This implements the "pg_catalog.pg_tablespace_location()" function.
	; See https://pgpedia.info/p/pg_tablespace_location.html for more details.
	; Since Octo currently does not currently support tablespaces, this function currently returns NULL always.
	QUIT $ZYSQLNULL

pgGetRuleDef(ruleoid,pretty)	; TODO this is just a placeholder
	; This implements the "pg_catalog.pg_get_ruledef()" function.
	; See https://pgpedia.info/p/pg_get_ruledef.html for more details.
	; Octo currently returns the empty string for the rule definition.
	QUIT ""

