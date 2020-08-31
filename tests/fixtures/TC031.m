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

TC031	;
	; TC031 : OCTO587 : SIG-11 when executing a lot of CREATE TABLE queries
	;
	; Helper M program that generates a .sql file with thousands of CREATE TABLE queries that simulate a VistA DDL
	; specification (which contains 7042 CREATE TABLE queries). This lets us recreate OCTO587 without having to
	; maintain the huge VistA DDL file (~10Mib) in the git repository.
	;
        NEW i,j,numcols
        FOR i=1001:1:10000 do
        . WRITE "CREATE TABLE TABLE"_i_"(",!
        . WRITE "id INTEGER PRIMARY KEY,",!
        . SET numcols=2**$RANDOM(6)
        . FOR j=1:1:numcols do
        . . WRITE " COL"_j_" INTEGER GLOBAL ""^COL"_j_"(""""abcd"""",2,""""efgh"""")"" PIECE "_j_$SELECT(j'=numcols:",",1:""),!
        . WRITE ");",!
        QUIT
