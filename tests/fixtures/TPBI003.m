;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; This M program is used by the TPBI003 subtest to generate various SQL commands and verify that
; "atttypmod" column in the "pg_attribute" table is maintained in Octo just like it is in Postgres.
; -----------------------------------------------------------------------------------------------------

TPBI003;
	set relname="tpbi003",attname="id"
	set x("numeric")=""
	set x("numeric(1)")=""
	set x("numeric(1,0)")=""
	set x("numeric(1,1)")=""
	set x("numeric(2,0)")=""
	set x("numeric(2,1)")=""
	set x("numeric(2,2)")=""
	set x("varchar")=""
	set x("varchar(1)")=""
	set x("varchar(2)")=""
	set subs="" for  set subs=$order(x(subs),1)  quit:subs=""  do
	. write "create table "_relname_" (id "_subs_");",!
	. write "select atttypmod from pg_attribute where attrelid = (select oid from pg_class where relname = '"_relname_"')"
	. write " and attname = '"_attname_"';",!
	. write "drop table "_relname_";",!
	quit

