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

; This program returns a random string literal that is treated as a boolean TRUE or FALSE (depending on the label invoked).
; This is to conform to Postgres behavior where 't', 'true', 'y', 'yes', '1' are treated as TRUE and
; 'f', 'false', 'n', 'no', '0' are treated as FALSE.

TBT12	;
	quit

false	;
	new rand
	set rand=$random(5)
	write $select(rand=0:"'f'",rand=1:"'false'",rand=2:"'n'",rand=3:"'no'",1:"'0'"),!
	quit

true	;
	new rand
	set rand=$random(5)
	write $select(rand=0:"'t'",rand=1:"'true'",rand=2:"'y'",rand=3:"'yes'",1:"'1'"),!
	quit

