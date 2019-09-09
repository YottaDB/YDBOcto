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

; Generates a query that is 32k characters long
; and some regular queries at the end
gt32k ;
	open "in.sql":(stream:nowrap)
	use "in.sql":nowrap
	set o="select"
	set o=o_$c(10)_"a"
	for i=1:1:(32768/2) set o=o_(i#10)
	set o=o_$c(10)_",b"
	for i=1:1:(32768/2) set o=o_(i#10)
	set o=o_$c(10)_"from names; select notathing from names; select firstname from names limit 1;"
	set o=o_$c(10)_"select lastname from names limit 1;"
	write o,!
	quit

; Generates a query that is almost 32k characters long
; and the next queries on the same line push it over 32k
almost32k ;
	open "in.sql":(stream:nowrap)
	use "in.sql":nowrap
	set o="select"
	set o=o_$c(10)_"a"
	for i=1:1:(32740/2) set o=o_(i#10)
	set o=o_$c(10)_",b"
	for i=1:1:(32740/2) set o=o_(i#10)
	set o=o_$c(10)_"from names; select notathing from names; select firstname from names limit 1;"
	set o=o_$c(10)_"select lastname from names limit 1;"
	write o,!
	quit

; Generates a query that is 10k characters long
; then generates a query that is 32k characters long
; so that memory addresses will overlap when shifting the second query
; in the input buffer
bufoverlap ;
	open "in.sql":(stream:nowrap)
	use "in.sql":nowrap
	set o="select"
	set o=o_$c(10)_"a"
	for i=1:1:(10000/2) set o=o_(i#10)
	set o=o_$c(10)_",b"
	for i=1:1:(10000/2) set o=o_(i#10)
	set o=o_$c(10)_"from names;"
	write o,!
	set o="select"
	set o=o_$c(10)_"a"
	for i=1:1:(32768/2) set o=o_(i#10)
	set o=o_$c(10)_",b"
	for i=1:1:(32768/2) set o=o_(i#10)
	set o=o_$c(10)_"from names;"
	set o=o_$c(10)_"select * from names limit 1;"
	write o,!
	quit

gt10mb ;
	open "in.sql":(stream:nowrap)
	use "in.sql":nowrap
	set o="select"
	for i=1:1:4096 write o,! set o="" for k=1:1:21 set o=o_"a1234567890,b1234567890,c1234567890,"
	write o set o=""
	set o=o_$c(10)_"a1234567890,b1234567890,c1234567890"
	set o=o_$c(10)_"from largecolnames;"
	write o,!
	quit

gt10mbOutA ;
	open "output.ref":(stream:nowrap)
	use "output.ref":nowrap
	set o="OCTO> select"
	for i=1:1:4096 write o,! set o="OCTO> " for k=1:1:21 set o=o_"a1234567890,b1234567890,c1234567890,"
	write o set o=""
	set o=o_$c(10)_"OCTO> a1234567890,b1234567890,c1234567890"
	set o=o_$c(10)_"OCTO> from largecolnames;"
	write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"1|2|3|"
	set o=o_"1|2|3" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"4|5|6|"
	set o=o_"4|5|6" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"7|8|9|"
	set o=o_"7|8|9" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"10|11|12|"
	set o=o_"10|11|12" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"13|14|15|"
	set o=o_"13|14|15" write o,!
	set o="OCTO> "
	write o
	quit

gt10mbOutB ;
	open "output.ref":(stream:nowrap)
	use "output.ref":nowrap
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"1|2|3|"
	set o=o_"1|2|3" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"4|5|6|"
	set o=o_"4|5|6" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"7|8|9|"
	set o=o_"7|8|9" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"10|11|12|"
	set o=o_"10|11|12" write o,!
	set o="" for i=1:1:4096 for k=1:1:21 set o=o_"13|14|15|"
	set o=o_"13|14|15" write o,!,!
	quit
