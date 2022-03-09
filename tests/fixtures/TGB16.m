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
; -- TGB16: OCTO812 : Test sub-query in WHERE clause doing GROUP BY on parent query column
;
; This program generates random SQL queries to test the above use case.
; This program is modeled on tests/fixtures/TJ0C08.m but tailored to testing YDBOcto#812.
; -----------------------------------------------------------------------------------------------------

genrandomqueries	;
	set numqueries=20	; generate 20 queries so as not to take a long time for this test to run in pipeline
	set q=0
	for  do  quit:$increment(q)=numqueries
	. set sqlquery="select * from "_$$table_" n0"
	. ; Add WHERE
	. set sqlquery=sqlquery_" where "_$$exists_" (select "_$$selectcolumnlist_" from "_$$table_" n1"
	. set sqlquery=sqlquery_" where "_$$whereclause
	. set sqlquery=sqlquery_" group by "_$$groupbyclause
	. set sqlquery=sqlquery_");"
	. write sqlquery,!
	quit
	;

table()
	new table
	set tabletype=$random(2)
	quit $select(tabletype=0:"names",1:"(select * from names)")

exists()
	quit $select($random(2)=0:"exists",1:"not exists")

selectcolumnlist()
	new i,ncols,selectcolumnlist
	set ncols=1+$random(2)
	set selectcolumnlist=""
	for i=1:1:ncols do
	. new selectcolumn
	. set selectcolumn=$$tablename_"."_$$column
	. if selectcolumn["n1" set selectcolumn=$$aggregatefunction(selectcolumn)
	. set:i'=ncols selectcolumn=selectcolumn_", "
	. set selectcolumnlist=selectcolumnlist_selectcolumn
	quit selectcolumnlist

whereclause()
	new whereclause
	set whereclause=$random(3)
	quit $select(whereclause=0:"false",whereclause=1:"true",1:"null")

groupbyclause()
	new i,ncols,groupbylist
	set ncols=1+$random(2)
	set groupbylist=""
	for i=1:1:ncols do
	. new groupbycolumn
	. set groupbycolumn=$$tablename_"."_$$column
	. set:i'=ncols groupbycolumn=groupbycolumn_","
	. set groupbylist=groupbylist_groupbycolumn
	quit groupbylist

tablename()
	new tablename
	quit "n"_$random(2)

column()
	new columnname
	set columnname=$random(4)
	quit $select(columnname=0:"id",columnname=1:"firstname",columnname=2:"lastname",1:"*")

aggregatefunction(selectcolumn)
	new aggregatefunction
	set aggregatefunction=$random(3)
	set aggregatefunction=$select(aggregatefunction=0:"COUNT",aggregatefunction=1:"MIN",1:"MAX")
	set:selectcolumn["*" aggregatefunction="COUNT" ; If tablename.* usage, then canot use MIN/MAX. Only COUNT can be used
	quit aggregatefunction_"("_selectcolumn_")"

