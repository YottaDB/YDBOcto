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

run
  ; Cleanup global variables and relevant triggers when CancelRequest received during cross reference generation
  ; Note: _ydboctoX*.m has the corresponding sets of %ydboctoCancel lvn which are used here.
  NEW tableName,columnName
  SET tableName="",columnName=""
  FOR  SET tableName=$ORDER(%ydboctoCancel(tableName)) QUIT:""=tableName  DO
  . FOR  SET columnName=$ORDER(%ydboctoCancel(tableName,columnName)) QUIT:""=columnName  DO
  . . IF $DATA(%ydboctoCancel(tableName,columnName,"Trigger")) DO
  . . . IF $$dollarZTRIGGER^%ydboctoplanhelpers("item",%ydboctoCancel(tableName,columnName,"Trigger"))
  . . KILL:$DATA(%ydboctoCancel(tableName,columnName,"Node1")) @%ydboctoCancel(tableName,columnName,"Node1")
  . . KILL:$DATA(%ydboctoCancel(tableName,columnName,"Node2")) @%ydboctoCancel(tableName,columnName,"Node2")
  KILL %ydboctoCancel
  ; Set to let run_query know the query was canceled
  SET %ydboctoCancel=1
  ; Release any existing locks as they are no longer needed for the canceled query
  LOCK
  QUIT
