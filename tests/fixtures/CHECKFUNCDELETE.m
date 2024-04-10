;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECKFUNCDELETE
	new funcname,funchash,funchasharr
	set funcname=$zcmdline
	WRITE "Remaining function definitions (hashes):",!
	SET funchash="" FOR  SET funchash=$order(^%ydboctoocto("functions",funcname,funchash)) QUIT:funchash=""  DO
	. WRITE "--> Found: ^%ydboctoocto(""functions"","""_funcname_""","""_funchash_""")",!
	. SET funchasharr(funchash)=""
	DO:$DATA(^%ydboctoocto("tables","pg_catalog","pg_proc",$GET(^%ydboctoocto("functions",funcname,$order(funchasharr(funchash)),"oid"))))=0
	. WRITE "Successfully deleted function """_funcname_""" from pg_catalog.pg_proc",!
	quit
