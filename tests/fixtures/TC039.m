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

TC039	;
	; TC039 : OCTO590 : Dynamically resize buffers when storing text DDL definitions > OCTO_INIT_BUFFER_LEN
	;
	; Helper M program that generates a .sql file containing a CREATE TABLE statement with a thousand columns
	; exceeding OCTO_INIT_BUFFER_LEN to validate buffer expansion in emit_column_specification and
	; emit_create_table.
	NEW i,numcols
	WRITE "CREATE TABLE TC039(",!
	WRITE "id INTEGER PRIMARY KEY,",!
	SET numcols=1000
	FOR i=1:1:numcols DO
	. WRITE " COL"_i_" INTEGER GLOBAL ""^COL"_i_"(""""abcd"""",keys(""""ID""""),""""efgh"""")"" PIECE "_i_$SELECT(i'=numcols:",",1:""),!
	WRITE ");",!
	QUIT
