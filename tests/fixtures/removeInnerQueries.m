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
	new len,i,state,result,queryfile,query
	set queryfile=$zcmdline
	open queryfile:(readonly)
	use queryfile
	read query
	close queryfile
	set len=$zlength(query),state=0,result=""
	for i=1:1:len do
	. new ch
	. set ch=$zextract(query,i)
	. ; state = 0 implies we are in the outermost query
	. ; state > 0 implies we are inside an inner query
	. if "'"=ch do
	. . new endReached,j
	. . set endReached=0
	. . ; Saw a single-quote. Determine the end of this single-quoted string.
	. . ; Treat 2 consecutive single-quotes as a single quote within the string.
	. . for j=i+1:1:len do  quit:endReached
	. . . set ch=$zextract(query,j)
	. . . if ("'"=ch) do  quit:endReached
	. . . . if $increment(j)
	. . . . if (j<=len) do
	. . . . . if ("'"=$zextract(query,j))
	. . . . . else  set endReached=1
	. . if state=0 set result=result_$zextract(query,i,j-1)
	. . ; else: We are already in a sub-query. Ignore this single-quote surrounded string literal completely
	. . set i=j-1
	. else  if "("=ch set state=state+1
	. else  if ")"=ch set state=state-1
	. else  set:state=0 result=result_ch
	write result,!
	quit
