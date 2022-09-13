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
  NEW rtn,zcmpl
  SET rtn="^"_routine,zcmpl=$ZCOMPILE
  SET:'$ZFIND($ZCONVERT($zcompile,"l"),"-noline_entry") $ZCOMPILE=$ZCOMPILE_" -noline_entry"
  DO @rtn@(cursorId,wrapInTp)
  SET $ZCOMPILE=zcmpl
  QUIT
  ;
xrefMetadata(routine) ; Octo entry point to get AIM xref metadata before creating xref
  ; The metadata contains the location of the AIM global in
  ; ^%ydbAIMOctoCache(<table>,<column>,"location") so that we can inline it in generated plans.
  ; The metadata also contains comments, and cancellation information
  ; Note: "routine" variable is of the form "%ydboctoXOyAkV0dwqVINYJD702SbAA"
  ;   where the generated M file name is "_ydboctoXOyAkV0dwqVINYJD702SbAA.m".
  ;
  ; The xref routine name is dependent on the names of the table and column corresponding to the xref.
  ; This means it is possible 2 unrelated tables could have the same table and column name but a completely
  ; different structure otherwise and end up with the same xref plan name. If we create one such table, run
  ; a select query that creates the xref M routine and then drop the table and then create the second unrelated
  ; table and run a selecct query that again uses the xref M routine with the same name (but different contents)
  ; we would come here again and the DO call below would use the already loaded M routine from the first table
  ; leading to incorrect query results. Therefore we do an explicit ZLINK to ensure the new M routine contents
  ; are linked/loaded into the process address space (YDBOcto#904) before the DO call below. Since the routine
  ; name can at most be 31 characters long, we use that below as YDB_MAX_IDENT macro cannot be used in M code.
  ZLINK "_"_$ZEXTRACT(routine,2,31)
  DO xrefMetadata^@routine
  QUIT
