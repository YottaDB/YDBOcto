;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	;
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
  NEW tableName,columnName
  SET tableName="",columnName=""
  FOR  SET tableName=$ORDER(%ydboctoCancel(tableName)) QUIT:""=tableName  DO
  . FOR  SET columnName=$ORDER(%ydboctoCancel(tableName,columnName)) QUIT:""=columnName  DO
  . . IF $ZTRIGGER("item",%ydboctoCancel(tableName,columnName,"Trigger"))
  . . KILL @%ydboctoCancel(tableName,columnName,"Node")
  KILL %ydboctoCancel
  ; Set to let run_query know the query was canceled
  SET %ydboctoCancel=1
  ; Release any existing locks as they are no longer needed for the canceled query
  LOCK
  QUIT
