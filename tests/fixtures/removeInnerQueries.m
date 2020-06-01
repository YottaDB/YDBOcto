;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

removeInnerQueries;
	; Removes all Inner Queries from a query
	; For example "select 1 from names where 1 != (select 2 from names LIMIT 1) ORDER BY 1"
	; Would be transformed to "select 1 from names where 1 != ORDER BY 1".
	; This is used by the caller to determine if ORDER BY exists in the outer most query or not.
	new len,i,state,result,queryfile,query,quit
	set queryfile=$zcmdline
	open queryfile:(readonly)
	use queryfile
	read query
	close queryfile
	set len=$zlength(query),state=0,result=""
	for i=1:1:len do
	. new ch
	. set ch=$zextract(query,i)
	. ; state = -1 implies we are inside a single-quoted string
	. ; state > 0 implies we are inside an inner query
	. if "'"=ch do
	. . if state=0 set state=-1
	. . else  if state=-1 set state=0
	. if (-1'=state) do  quit:quit
	. . set quit=0
	. . if "("=ch set state=state+1,quit=1
	. . if ")"=ch set state=state-1,quit=1
	. quit:state>0
	. set result=result_ch
	write result,!
	quit
