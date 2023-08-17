;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The below is a copy of genquery.m from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1005#description
; It is supplied the LEFT JOIN depth as the first parameter in $zcmdline (something like 20) and a second
; boolean parameter of 1 indicating key fixing optimization has to be enabled.
;
; It also tests the query at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1006#note_1517495135 when
; supplied the LEFT JOIN depth as the first parameter in $zcmdline (something like 20) and a second
; boolean parameter of 0 indicating key fixing optimization has to be disabled.

TOJ11	;
	write "select count(*)",!
	write "from names n0",!
	set leftjoins=$piece($zcmdline," ",1)
	set keyfix=$piece($zcmdline," ",2)
	for i=1:1:$zcmdline do
	.	write "left join names n",i," ON n",i,".id = n",(i-1),".id"
	.	write:'keyfix "+1"
	.	write !
	write ";",!
	quit

