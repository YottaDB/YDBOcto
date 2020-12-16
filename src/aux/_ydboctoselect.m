;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

run(cursorId,routine)
  ; Note: "routine" variable is of the form "%ydboctoPOyAkV0dwqVINYJD702SbAA"
  ; where the generated M file name is "_ydboctoPOyAkV0dwqVINYJD702SbAA.m".
  ; We need to prefix a "^" to it before invoking the M program using "DO" with entryref indirection.
  NEW rtn
  SET rtn="^"_routine
  TSTART ():(serial)
  DO @rtn@(cursorId)
  TCOMMIT
  QUIT
