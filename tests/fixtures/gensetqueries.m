;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --------------------------------------------------------------------------------------------------------------------
; This program generates a few random simple SQL queries (with optional boolean expressions (OR/AND etc.), ORDER BY,
; and LIMIT usage each), that in turn are connected with SET operations (UNION/INTERSECT/EXCEPT etc.) and ORDER BY.
; Certain features of this test are currently disabled due to pending issues.
; --------------------------------------------------------------------------------------------------------------------

gensetqueries	;
	set numqueries=30	; generate 30 queries so as not to take a long time for this test to run in pipeline
	set q=0
	set setoper(0)="union",setoper(1)="union all"
	set setoper(2)="intersect",setoper(3)="intersect all"
	set setoper(4)="except",setoper(5)="except all"
	for q=1:1:numqueries do
	. set sqlquery=""
	. set numsimplequeries=2+$random(3)
	. for j=1:1:numsimplequeries do
	. . set sqlquery(j)="select name from easynames"
	. . if $random(8) do
	. . . ; generate WHERE
	. . . set sqlquery(j)=sqlquery(j)_" where "
	. . . set numbools=1+$random(3)	; Increasing this to $random(4) etc. causes occasional failures due to
	. . .				; differences in order of output between Octo and Postgres so limiting this to 3 for now
	. . . for k=1:1:numbools do
	. . . . set o=$random(3)
	. . . . set oper=$select(o=0:"=",o=1:"<",1:">")
	. . . . set sqlquery(j)=sqlquery(j)_"id "_oper
	. . . . set sqlquery(j)=sqlquery(j)_" "_$select(o=1:$random(4),o=2:12+$random(5),1:$random(17))
	. . . . set:k'=numbools sqlquery(j)=sqlquery(j)_" "_$select($random(4):"AND",1:"OR")_" "
	. . if $random(2) do
	. . . ; generate ORDER BY
	. . . set sqlquery(j)=sqlquery(j)_" order by name"
	. . ;
	. . ; If there is more than one boolean expression, we cannot reliably use LIMIT as Postgres and Octo could
	. . ; give different outputs in that case. This is because Postgres reorders the boolean expressions to go in
	. . ; order while Octo still does not do that. Example queries are
	. . ;	select name from easynames where id > 14 OR id = 13 limit 1
	. . ;	select name from easynames where id > 13 OR id < 3 limit 1
	. . ; The below reordered queries would work as operands for the OR are in sequence. But it is not easy to
	. . ; ensure that in this M program so we skip using limit if numbools is > 1 (i.e. OR or AND etc. is used).
	. . ;	select name from easynames where id = 13 OR id > 14 limit 1
	. . ;	select name from easynames where id < 3 OR id > 13 limit 1
	. . if (1=$get(numbools))&($random(2)) do
	. . . ; generate LIMIT
	. . . set sqlquery(j)=sqlquery(j)_" limit "_$random(17)
	. . set file="testhelper"_$translate($justify(q,2)," ","0")_"."_j_".sql"
	. . open file:(newversion)  use file
	. . write sqlquery(j),!
	. . if j'=1 do
	. . . set setoper=$random(6)
	. . . set sqlquery=sqlquery_" "_setoper(setoper)_" "
	. . close file
	. . set sqlquery=sqlquery_"("_sqlquery(j)_")"
	. . kill row(row)  if $incr(row,-1)
	. set file="settest"_$translate($justify(q,2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write sqlquery,!
	. close file
	quit

genfixedsetqueries	;
	set query($incr(query))="(select firstname from names limit 2) union (select lastname from names limit 3);"
	set query($incr(query))="(select firstname from names limit 2) union (select lastname from names limit 4);"
	set query($incr(query))="(select firstname from names limit 2) union (select lastname from names limit 1);"
	set query($incr(query))="(select lastname from names limit 2) union (select lastname from names limit 3);"
	set query($incr(query))="(select lastname from names limit 2) union (select lastname from names limit 4);"
	set query($incr(query))="(select lastname from names limit 2) union (select lastname from names limit 1);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 order by firstname limit 2) union (select firstname from names where id = 1 OR id > 3 order by firstname limit 3);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 order by firstname limit 3) union (select firstname from names where id = 1 or id = 2 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 limit 2) union (select firstname from names where id = 1 OR id > 3 limit 3);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 limit 3) union (select firstname from names where id = 1 or id = 2 limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 limit 2) union (select firstname from names where id > 3 limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 order by firstname limit 2) union (select firstname from names where id = 1 OR id > 3 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 order by firstname limit 2) union (select firstname from names where id = 1 or id = 2 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names order by firstname) union (select firstname from names where id = 1 OR id > 3 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 order by firstname limit 2) union (select firstname from names order by firstname);"
	set query($incr(query))="(select firstname from names order by firstname limit 3) union (select firstname from names where id > 3 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names where id > 3 order by firstname limit 2) union (select firstname from names order by firstname limit 3);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 order by firstname) union (select firstname from names where id = 1 OR id > 3 order by firstname limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 order by firstname limit 2) union (select firstname from names where id = 1 or id = 2 order by firstname);"
	set query($incr(query))="(select firstname from names order by firstname limit 3) union (select firstname from names where id > 3);"
	set query($incr(query))="(select firstname from names where id > 3) union (select firstname from names order by firstname limit 3);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2 limit 2) union (select firstname from names where id = 1 OR id > 3 limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 limit 2) union (select firstname from names where id = 1 or id = 2 limit 2);"
	set query($incr(query))="(select firstname from names) union (select firstname from names where id = 1 OR id > 3 limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 limit 2) union (select firstname from names);"
	set query($incr(query))="(select firstname from names limit 3) union (select firstname from names where id > 3 limit 2);"
	set query($incr(query))="(select firstname from names where id > 3 limit 2) union (select firstname from names limit 3);"
	set query($incr(query))="(select firstname from names where id = 1 or id = 2) union (select firstname from names where id = 1 OR id > 3 limit 2);"
	set query($incr(query))="(select firstname from names where id = 1 OR id > 3 limit 2) union (select firstname from names where id = 1 or id = 2);"
	set query($incr(query))="(select firstname from names limit 3) union (select firstname from names where id > 3);"
	set query($incr(query))="(select firstname from names where id > 3) union (select firstname from names limit 3);"
	set query($incr(query))="(select firstname from names limit 3) union (select firstname from names where id > 3) union (select firstname from names where id < 1 limit 1);"
	set query($incr(query))="(select firstname from names limit 3) union (select firstname from names where id < 1 limit 1) union (select firstname from names where id > 3);"
	set query($incr(query))="(select firstname from names where id > 3) union (select firstname from names limit 3) union (select firstname from names where id < 1 limit 1);"
	for i=1:1:query do
	. set file="setquery"_$translate($justify(i,2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write query(i),!
	. close file
	quit
