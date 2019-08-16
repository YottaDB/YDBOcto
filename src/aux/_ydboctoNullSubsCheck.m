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


run() ;
	NEW verified,octogbl,reglist,regnum,regname,numregs
	SET quit=0
	FOR octogbl="^%ydboctoxref","^%ydboctoocto","^%ydboctoschema" DO
	. SET reglist=$VIEW("REGION",octogbl),numregs=$LENGTH(reglist,",")
	. FOR regnum=1:1:numregs DO
	. . SET regname=$PIECE(reglist,",",regnum)
	. . QUIT:$DATA(verified(regname))
	. . IF ($$^%PEEKBYNAME("sgmnt_data.null_subs",regname)'=1) DO
	. . . WRITE "ERROR: Null subscripts must be enabled for global "_octogbl_" in region "_regname,!
	. . . SET quit=1
	. . SET verified(regname)=""
	QUIT quit
