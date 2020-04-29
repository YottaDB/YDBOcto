;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECKFUNCDELETE
	new funcname
	set funcname=$zcmdline
	IF $DATA(^%ydboctoocto("functions",funcname))=0 DO
	. WRITE "Successfully cleared ^%ydboctoocto(""functions"","""_funcname_""")",!
	IF $DATA(^%ydboctoocto("tables","pg_catalog","pg_proc",$GET(^%ydboctoocto("functions",funcname,"oid"))))=0 DO
	. WRITE "Successfully deleted function """_funcname_""" from pg_catalog.pg_proc",!
	quit
