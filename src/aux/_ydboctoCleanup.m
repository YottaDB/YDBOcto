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

run
  ; Cancellation with YDBAIM only throws aways the Octo-level metadata, rather than the entire AIM level cross-reference. This
  ; means that cancellation deletes ^%ydbAIMOctoCache(<table>,<column>) but keeps the ^%ydbAIMD* globals. The cross-reference stays
  ; in a half-built ; state and may be resumed by another query.
  ;
  ; Note: _ydboctoX*.m has the corresponding sets of %ydboctoCancel lvn which are used here.
  NEW tableName,columnName
  SET tableName="",columnName=""
  FOR  SET tableName=$ORDER(%ydboctoCancel(tableName)) QUIT:""=tableName  DO
  . FOR  SET columnName=$ORDER(%ydboctoCancel(tableName,columnName)) QUIT:""=columnName  DO
  . . KILL:$DATA(%ydboctoCancel(tableName,columnName,"aimXref")) ^%ydbAIMOctoCache(tableName,columnName)
  . . KILL:$DATA(%ydboctoCancel(tableName,columnName,"lvnTableXref")) @%ydboctoCancel(tableName,columnName,"lvnTableXref")
  KILL %ydboctoCancel
  ; Set to let run_query know the query was canceled
  SET %ydboctoCancel=1
  ; Rollback TP transaction if active (note: we don't know of a case why it won't be active).
  TROLLBACK:$TLEVEL
  ; Release any existing locks as they are no longer needed for the canceled query
  LOCK
  QUIT
