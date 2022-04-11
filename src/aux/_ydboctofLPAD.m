;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implements the SQL LPAD function (left pad a string `str` with another string `padstr` up to a maximum number of characters `num`)
LPAD(str,num,padstr)
	quit

PostgreSQL(str,num,padstr)
	new result
	if (0'=$DATA(padstr))  do
	. set result=$$LPADhelper(str,num,padstr)
	else  do
	. set result=$$LPADhelper(str,num)
	quit result

MySQL(str,num,padstr)
	new result
	if (0'=$DATA(padstr))  do
	. set result=$$LPADhelper(str,num,padstr)
	else  do
	. set %ydboctoerror("UNKNOWNFUNCTION",1)="LPAD"	; pass parameter to `src/ydb_error_check.c`
	. set %ydboctoerror("UNKNOWNFUNCTION",2)="2"	; pass parameter to `src/ydb_error_check.c`
	. set %ydboctoerror("UNKNOWNFUNCTION",3)="MYSQL"	; pass parameter to `src/ydb_error_check.c`
	. zmessage %ydboctoerror("UNKNOWNFUNCTION")
	. set result=$ZYSQLNULL
	quit result

LPADhelper(str,num,padstr)
	; Return `str` padded to `num` spaces. If `num` is less than the length of the string, truncate to `num` characters instead.
	new result,len,pad,padlen,i
	set pad=""
	quit:$ZYISSQLNULL(str)!$ZYISSQLNULL(num)!$ZYISSQLNULL(padstr) $ZYSQLNULL
	if ($DATA(padstr))  do
	. set len=$length(str)
	. if (num<len)  do
	. . set result=$extract(str,0,num)
	. else  do
	. . set padstrlen=$length(padstr)
	. . set padlen=num-len
	. . if (0<padstrlen)  do
	. . . for i=0:padstrlen:padlen do
	. . . . set pad=pad_padstr
	. . set result=$extract(pad,0,padlen)_str
	else  do
	. set result=$extract($justify(str,num),0,num)
	quit result
