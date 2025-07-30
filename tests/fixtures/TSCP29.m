;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2024-2025 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setupdev ; Set-up Principal Device so that long strings are not split (using newlines) at 32K byte boundary
	; This magic incantation works for output redirected to files (yottadb -r foo > foo.txt)
	use $principal:nowrap
	quit

; Following are individually called by TSCP29
test4
	; 32k length string where aaaa is replaced as bbb
	do setupdev
	set res=""
	for i=0:1:32000 set res=res_"a"
	write "select replace('"_res_"','aaaa','bbb');"
	quit

test5
	; 32k length string where abcabc is replaced as xyzxyz
	do setupdev
	set res=""
	for i=0:1:1000 set res=res_"abcabc"
	write "select replace('"_res_"','abcabc','xyzxyz');"
	quit

test6
	; 1k length utf string each char replacement
	do setupdev
	set res=""
	for i=0:1:1000 set res=res_$char(20028)
	write "select replace('"_res_"','"_$char(20028)_"','"_$char(20027)_"');"
	quit

test7
	; 1k length utf string 2 char replacement
	do setupdev
	set res=""
	for i=0:1:1000 set res=res_$char(20028)
	write "select replace('"_res_"','"_$char(20028)_$char(20028)_"','"_$char(20027)_$char(20027)_"');"
	quit

test8
	; 32k length utf string each char replacment
	do setupdev
	set res=""
	for i=0:1:32000 set res=res_$char(20028)
	write "select replace('"_res_"','"_$char(20028)_"','"_$char(20027)_"');"
	quit

test9
	; 32k length utf string double char replacment
	do setupdev
	set res=""
	for i=0:1:32000 set res=res_$char(20028)
	write "select replace('"_res_"','"_$char(20028)_$char(20028)_"','"_$char(20027)_$char(20027)_"');"
	quit
