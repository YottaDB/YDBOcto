;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	;
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
  ; * "wrapInTp" is 0 or 1. If 1, the query execution (excluding building any cross references) is wrapped in a TP transaction.
  NEW rtn
  SET rtn="^"_routine
  DO @rtn@(cursorId,wrapInTp)
  QUIT
  ;
xrefMetadata(routine) ; Octo entry point to get AIM xref metadata before creating xref
  ; The metadata contains the location of the AIM global in
  ; ^%ydbAIMOctoCache(<table>,<column>,"location") so that we can inline it in generated plans.
  ; The metadata also contains comments, and cancellation information
  ; Note: "routine" variable is of the form "%ydboctoXOyAkV0dwqVINYJD702SbAA"
  ;   where the generated M file name is "_ydboctoXOyAkV0dwqVINYJD702SbAA.m".
  DO xrefMetadata^@routine
  QUIT

