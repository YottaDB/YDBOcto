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
; This program splits an input file containing multiple SQL queries into multiple files each containing 1 query.
; Assumes that each query is contained in 1 line (i.e. not split across multiple lines).
; --------------------------------------------------------------------------------------------------------------------

splitqueries;
	new line,nlines,queryfile,i,cnt,file
	set queryfile=$piece($zcmdline," ",1)
	open queryfile:(readonly)
	use queryfile
	for  read line($increment(nlines))  quit:$zeof
	close queryfile
	kill line(nlines)  if $increment(nlines,-1) ;kills line that contains EOF
	for i=1:1:nlines do
	. ; Filters out comment lines (start with '--' or '#'), and/or blank lines
	. if (($extract(line(i),1)'="#")&($extract(line(i),1)'="")&($extract(line(i),1,2)'="--")) do
	. . set file=$piece(queryfile,".",1)_"-"_$translate($justify($increment(cnt),2)," ","0")_".sql"
	. . open file:(newversion)  use file
	. . write line(i),!
	. . close file
	quit
