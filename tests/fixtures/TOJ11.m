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

TOJ11	;
	write "select count(*)",!
	write "from names n0",!
	for i=1:1:$zcmdline do
	.	write "left join names n",i," ON n",i,".id = n",(i-1),".id",!
	write ";",!
	quit

