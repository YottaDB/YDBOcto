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

; User defined routines that can be called from octo. Test various
; return types for extrinsic functions declared by CREATE FUNCTION.
;
RETTRUE()     ;
        quit 1


RETTRUESTR()     ;
        quit "true"


RETFALSE()     ;
        quit 0


RETFALSESTR()     ;
        quit "false"


RETINT(int)     ;
        quit int


RETNUM(num)     ;
        quit num


RETVARCHAR(varchar)     ;
        quit varchar


RETVARCHAR2(numeric,varchar)     ;
        quit numeric_varchar
