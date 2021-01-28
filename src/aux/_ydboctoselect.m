;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

run(cursorId,routine,wrapInTp)
  ; * "routine" variable is of the form "%ydboctoPOyAkV0dwqVINYJD702SbAA"
  ;   where the generated M file name is "_ydboctoPOyAkV0dwqVINYJD702SbAA.m".
  ;   We need to prefix a "^" to it before invoking the M program using "DO" with entryref indirection.
  ; * "wrapInTp" is 0 or 1. If 1, the query is wrapped in a TP transaction.
  NEW rtn
  SET rtn="^"_routine
  TSTART:wrapInTp ():(serial)
  DO @rtn@(cursorId)
  TCOMMIT:wrapInTp
  QUIT
