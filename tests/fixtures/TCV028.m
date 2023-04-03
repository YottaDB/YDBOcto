;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Helper function used by TCV028 subtest in test_create_view.bats.in

TCV028	;
	set numviews=5,numtables=3,numqueries=20
	for i=numviews:-1:1 do
	. write "drop view if exists TCV028",i,";",!
	for i=1:1:numviews do
	. write "create view TCV028",i
	. write " as select * from "
	. if i=1 do
	. . for j=1:1:numtables write:(1'=j) " natural join " write "names as n"_j
	. . write ";"
	. else   write "TCV028",(i-1)," as n0 "_$$randsetoper_" select * from TCV028",(i-1)," as n1;"
	. write !
	. write "select count(*) from TCV028"_i_";",!
	. write "select * from TCV028"_i_";",!
	for i=1:1:numqueries do
	. set nviews=1+$random(numviews)
	. write "select * from "
	. new jointype
	. for j=1:1:nviews do
	. . write:$data(jointype) " ",jointype," "
	. . write "TCV028",(1+$random(numviews))," as n",j
	. . do:$data(jointype)&((jointype="inner join")!(jointype="left join")!(jointype="right join"))
	. . . write " on n",(j-1),".id = n",j,".id"
	. . set jointype=$$randjointype(j)
	. write ";",!
	for i=numviews:-1:1 do
	. write "drop view if exists TCV028",i,";",!
	quit

randjointype(j);
	new rand
	set rand=$random(3)
	quit:rand=0 "cross join"
	quit:rand=1 "inner join"
	quit:rand=2 "left join"
	quit "right join"

randsetoper();
	new rand
	set rand=$random(6)
	quit:rand=0 "UNION"
	quit:rand=1 "UNION ALL"
	quit:rand=2 "INTERSECT"
	quit:rand=3 "INTERSECT ALL"
	quit:rand=4 "EXCEPT"
	quit "EXCEPT ALL"

