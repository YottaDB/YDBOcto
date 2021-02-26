;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --------------------------------------------------------------------------------------------------------------------
; This program generates a few random simple SQL queries to test set operations UNION/INTERSECT/EXCEPT etc. with NULL.
; --------------------------------------------------------------------------------------------------------------------

TSO15	;
	do init
	set numqueries=30	; generate 30 queries so as to limit test run time
	set setoper(0)="union",setoper(1)="union all"
	set setoper(2)="intersect",setoper(3)="intersect all"
	set setoper(4)="except",setoper(5)="except all"
	set setoper=6
	set numsubqueries=2+$random(2)
	for q=1:1:numqueries do
	. set sqlquery=""
	. set isstring=$random(2)	; choose STRING or NUMERIC type
	. for j=1:1:numsubqueries do
	. . set sqlquery(j)=$$subquery(isstring)
	. . if j'=1 do
	. . . set sqlquery=sqlquery_" "_setoper($random(setoper))_" "
	. . set sqlquery=sqlquery_"("_sqlquery(j)_")"
	. ; Randomly use the above subquery inside the IN operator to test columnkeyUNION^%ydboctoplanhelpers etc.
	. set:$random(2) sqlquery="select * from names where "_$$getelem(isstring)_$select($random(2):" NOT IN",1:" IN")_" ("_sqlquery_")"
	. write sqlquery,";",!
	quit

init	;
	set num(0)=1,num(1)=2,num(2)=3,num(3)="NULL::INTEGER",num=4
	; TODO: Set str(0)="''" (instead of the current value of NULL) once YDBOcto#687 is fixed.
	set str(0)="NULL",str(1)="'a'",str(2)="'b'",str(3)="NULL::VARCHAR",str=4
	quit

subquery(isstring) ;
	new i,numelems,query
	set numelems=1+$random(4)
	set query=""
	for i=1:1:numelems do
	. if i'=1 set query=query_" union all "
	. set query=query_"select "_$$getelem(isstring)
	quit query

getelem(isstring)	;
	quit $select('isstring:num($random(num)),1:str($random(str)))
	quit
