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

TC089	;
	; TC089 : OCTO1146 : Expose table / column context to an EXTRACT column's M routine
	;
	; Every entry point below is invoked from an EXTRACT column expression (or, for "nocontext",
	; from a CREATE FUNCTION extrinsic) and folds what the accessors reported into its return
	; value, so the query results themselves show the context each invocation ran with.
	QUIT

ctx(val)	; Report the table and the column this EXTRACT column is being computed for.
	QUIT val_" [tbl="_$$tableName^%ydboctoplanhelpers()_",col="_$$columnName^%ydboctoplanhelpers()_"]"

col(val)	; Report only the column name. Keeps the nested-EXTRACT output short enough to read.
	QUIT val_"<"_$$columnName^%ydboctoplanhelpers()_">"

nocontext(val)	; Invoked through CREATE FUNCTION, i.e. NOT from an EXTRACT column, and so not from under an
		; "octoExtractNN" label. Both accessors must read as empty here rather than reporting
		; whatever the last EXTRACT column in the same query happened to leave behind.
	QUIT val_" [tbl="_$$tableName^%ydboctoplanhelpers()_",col="_$$columnName^%ydboctoplanhelpers()_"]"

trap(val)	; Raise a runtime error and catch it one frame above the "octoExtractNN" label that NEWed
		; %ydboctoctx. Both the trap handler (still under the label) and the rest of the query
		; (after the label QUITs) must see the right column.
	NEW $ETRAP,res
	SET $ETRAP="SET $ECODE="""" SET res=""caught@""_$$columnName^%ydboctoplanhelpers() QUIT:$QUIT res  QUIT"
	SET res=$$boom^TC089(val)
	QUIT val_" ["_res_"]"

boom(val)	; Raise a DIVZERO from inside an EXTRACT column's expression.
	QUIT val/0
