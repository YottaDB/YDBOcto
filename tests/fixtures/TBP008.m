;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TBP008	;
	set npieces=$zlength($zcmdline," ")
	set firsttime=$piece($zcmdline," ",1)
	for piece=2:1:npieces do
	. set cputime=$piece($zcmdline," ",piece)
	. if (firsttime*4)<cputime do
	. . write "TEST-E-FAIL: CPU time #",piece," = [",cputime,"] is greater than 4 times CPU time #1 = [",firsttime,"]",!
	. else  write "PASS : Check of CPU time #",piece,!
	quit

