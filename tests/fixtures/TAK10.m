;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; This program generates SQL queries with random column names after AS in SELECT column list
; Some of the random names could end up being "by", "in" etc. which are SQL keywords.
; We expect the query to run fine irrespective of whether the AS column name is a keyword or not.
; -----------------------------------------------------------------------------------------------------

TAK10	;
	set base="abcdefghijklmnopqrstuvwxyz_"
	set baselen=$length(base)
	for i=1:1:100 do
	. ; Limit column name to 5 characters as it is very unlikely to run into valid keyword with longer column names
	. set len=$random(5)+1
	. set str=""
	. for j=1:1:len set str=str_$extract(base,$random(baselen)+1)
	. write "select 1 as ",str,";",!
	quit

