;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

loadbigdata
	; -------------------------------------------------------------------
	; Load a large amount of dummy data for tests requiring slow queries
	; -------------------------------------------------------------------
	kill ^names
	do init
	for i=1:4:numrows set ^names(i)="A|B"
	for i=2:4:numrows set ^names(i)="C|B"
	for i=3:4:numrows set ^names(i)="A|C"
	for i=4:4:numrows set ^names(i)="B|A"
	set subtest=$zcmdline
	if "TCR04"=subtest do
	. ; Generate a query file for use in the TCR04 subtest
	. set file=subtest_".sql"
	. open file:(newversion)
	. use file
	. write "insert into names select id+"_numrows_",firstname,lastname from names;",!
	. close file
	quit

verify	;
	new actualrows
	do init
	set actualrows=$order(^names(""),-1)
	if (actualrows'=numrows) write "VERIFY-E-FAIL : Expected ",numrows," rows but found ",actualrows,!
	else  write "PASS : Verification passed",!
	quit

init	;
	set numrows=1000000
	quit
