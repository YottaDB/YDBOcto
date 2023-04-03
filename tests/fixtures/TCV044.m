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

; -----------------------------------------------------------------------------------------------------
; Given a list of SELECT queries (in stdin), this M program generates a list of SELECT queries that
; randomly use VIEWS of the form TCV044_1, TCV044_2 etc. wherever it finds the NAMES table used.
; The list of VIEWS that it can use for substitution is found in the "common.viewlist" file.
; -----------------------------------------------------------------------------------------------------

TCV044	;
	set file="common.viewlist"
	open file:(readonly)
	use file
        for  read line($increment(nlines)) quit:$zeof
	close file
	kill line(nlines) if $increment(nlines,-1)
	merge views=line,views=nlines
	use $principal

	kill line,nlines
        for  read line($increment(nlines)) quit:$zeof
	kill line(nlines) if $increment(nlines,-1)
	for i=1:1:nlines do
	. for i2=1:1:1 do
	. . set len=$length(line(i)," names")
	. . set newquery=$piece(line(i)," names",1)
	. . for j=2:1:len do
	. . . set newquery=newquery_" "_views(1+$random(views))
	. . . set newquery=newquery_$piece(line(i)," names",j)
	. . ; set newquery=$extract(newquery,1,$length(newquery)-1)_" order by id;"
	. . write newquery,!

