;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pipe;
	new binpath
        set dev="pipe"
        open dev:(command="octo":exception="goto readeof")::"PIPE"
	use dev
	write "select * from names;",!
        for  read cmdop:1 quit:$zeof  quit:'$test  use $PRINCIPAL write cmdop,! use dev
	write "quit;",!
	close dev
	if $zclose'=0 write "TEST-E-FAIL",!
	quit

readeof;
	close dev
	write "TEST-E-FAIL",!
        quit
