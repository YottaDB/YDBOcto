;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TC041	;
	; TC041 : OCTO483 : Prohibit specification of more than 255 characters using DELIM, i.e. the max number of args for $CHAR
	;
	NEW i,numcols
	WRITE "CREATE TABLE TC041(",!
	WRITE "id INTEGER PRIMARY KEY,",!
	SET numcols=4
	FOR i=1:1:numcols DO
	. WRITE " COL"_i_" INTEGER GLOBAL ""^COL"_i_"(""""abcd"""",keys(""""ID""""),""""efgh"""")"" PIECE "_i_$SELECT(i'=numcols:",",1:""),!
	WRITE "DELIM ("
	FOR i=1:1:255 DO
	. WRITE i#127_","
	WRITE i#127_"));",!
	WRITE "SELECT * FROM TC041;",!
	QUIT
