;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; This program generates a random SQL query that optionally uses INNER or OUTER JOINs using the names database.
; This program is modeled on tests/fixtures/TOJ03.m but is a simpler version.
; -----------------------------------------------------------------------------------------------------

genrandomqueries	;
	set maxjoins=1+$random(4)
	set numqueries=20	; generate 20 queries so as not to take a long time for this test to run in pipeline
	set q=0
	for  do  quit:$increment(q)=numqueries
	. set numjoins=1+(q#maxjoins)	; can be 1-way, 2-way, 3-way, ... up to n-way join where n is specified through $zcmdline
	. set sqlquery="select * from "_$$table_" n0"
	. ; Add JOIN list
	. for i=1:1:numjoins  do
	. . set sqlquery=sqlquery_" "_$$jointype_" join "_$$table_" n"_i_" on "_$$onclause
	. ; Add WHERE
	. set sqlquery=sqlquery_" where exists (select "_$$tablename(numjoins)_"."_$$column_");"
	. write sqlquery,!
	quit
	;

table()
	new table
	set tabletype=$random(2)
	quit $select(tabletype=0:"names",1:"(select * from names)")

jointype()
	new jointype
	set jointype=$random(3)
	quit $select(jointype=0:"inner",jointype=1:"left",1:"right")

onclause()
	new onclause
	set onclause=$random(3)
	quit $select(onclause=0:"false",onclause=1:"true",1:"null")

tablename(numjoins)
	new tablename
	quit "n"_(1+$random(numjoins))

column()
	new columnname
	set columnname=$random(4)
	quit $select(columnname=0:"id",columnname=1:"firstname",columnname=2:"lastname",1:"*")

