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

; Helper function used by TCV048 subtest in test_create_view3.bats.in
; Generates a set of 'CREATE VIEW' commands involving random view names that depend on other random view names.

viewdependency	;
	do addname("names")
	for i=1:1:10 do
	. for  set name="v"_$random(20)  quit:'$data(namesxref(name))
	. write "create view "_name_" as select * from "_$$randviewname_" "_$$randsetoper_" select * from "_$$randviewname_";",!
	. do addname(name)
	quit

addname(name)	;
	set names($increment(names))=name
	set namesxref(name)=names
	quit

randsetoper();
	new rand
	set rand=$random(6)
	quit:rand=0 "UNION"
	quit:rand=1 "UNION ALL"
	quit:rand=2 "INTERSECT"
	quit:rand=3 "INTERSECT ALL"
	quit:rand=4 "EXCEPT"
	quit "EXCEPT ALL"

randviewname();
	quit names($random(names)+1)

