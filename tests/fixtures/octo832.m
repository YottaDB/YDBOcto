;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	;
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
	; Have a timed read for loop to ensure we exercise the EINTR code path that YDBOcto#832 fixed (see
	; https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/832#description for more details).
        for  read cmdop:1 quit:'$test  use $PRINCIPAL write cmdop,! use dev
	write "quit;",!
	; Have an untimed read for loop here to read any data that did not get read in the previous
	; timed read for loop due to say a loaded system etc.
        for  read cmdop use $PRINCIPAL write cmdop,! use dev
readeof	;
	close dev
	quit

