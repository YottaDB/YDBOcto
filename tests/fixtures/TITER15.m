;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TITER15	; YDBOcto#1146 : ITERATOR routines that ask which table/column they are advancing
	;
	; The iterators below generate the keys 1 and 2 at every level, so a table using them has
	; 4 rows regardless of what is in its GLOBAL. Each iterator records the (table,column) pair
	; that $$tableName^%ydboctoplanhelpers() and $$columnName^%ydboctoplanhelpers() reported
	; while it was running. The "iterctx" EXTRACT column reads those recordings back out, so
	; that the SELECT output can be compared against a reference file; it passes its own table
	; name in, so each section of the test reports only its own table's recordings.

catsys(n)	; ITERATOR for the first key column.
	DO note
	QUIT $SELECT(n>1:"",1:n+1)

id(catsys,n)	; ITERATOR for the second key column.
	DO note
	QUIT $SELECT(n>1:"",1:n+1)

note	; Record the context reported to the ITERATOR expression that is currently running.
	SET ^TITER15ctx($$tableName^%ydboctoplanhelpers(),$$columnName^%ydboctoplanhelpers())=""
	QUIT

seen(tbl)	; The (table,column) pairs recorded so far for table "tbl", space separated.
	NEW col,res
	SET res="",col=""
	FOR  SET col=$ORDER(^TITER15ctx(tbl,col)) QUIT:""=col  SET res=res_$SELECT(""=res:"",1:" ")_tbl_"."_col
	QUIT res

whoami()	; The EXTRACT column's own context. Proves the ITERATOR context did not leak into it.
	QUIT $$tableName^%ydboctoplanhelpers()_"."_$$columnName^%ydboctoplanhelpers()
