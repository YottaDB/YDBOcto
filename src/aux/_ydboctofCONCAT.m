;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022-2026 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implements the SQL CONCAT function, which concatenates
; the given strings into a single string.
; Concatenate str1, str2, and str3, if present.
; Returns the result of the concatenation, treating any
; NULL arguments as empty strings.
CONCAT(str1,str2,str3)
	new result
	set:$ZYISSQLNULL(str1) str1=""
	set result=str1
	if (0'=$DATA(str2))  do
	. set:$ZYISSQLNULL(str2) str2=""
	. set result=result_str2
	. if (0'=$DATA(str3))  do
	. . set:$ZYISSQLNULL(str3) str3=""
	. . set result=result_str3
	quit result
