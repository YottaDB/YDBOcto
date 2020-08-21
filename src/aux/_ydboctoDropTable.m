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

; This helper M function is invoked by "run_query" when a CREATE TABLE or DROP TABLE is executed.
; This takes care of the following cleanup (not done in C because deleting trigger is not yet supported in SimpleAPI).
;   1) Delete all generated cross references associated with columns in the table being created/deleted.
;   2) Delete all generated triggers associated with columns in the table being created/deleted.
;   3) Delete all M global nodes in ^%ydboctoocto/^%ydboctoschema that point to the above deleted objects.
;
dropTable(tableName)
  ; 1) Delete all generated cross references associated with columns in the table being created/deleted.
  ; 3) Delete all M global nodes in ^%ydboctoocto/^%ydboctoschema that point to the above deleted objects.
  KILL ^%ydboctoxref(tableName)
  ; 2) Delete all generated triggers associated with columns in the table being created/deleted.
  ; 3) Delete all M global nodes in ^%ydboctoocto/^%ydboctoschema that point to the above deleted objects.
  NEW column
  SET column="" FOR  SET column=$ORDER(^%ydboctoocto("xref_status",tableName,column))  QUIT:""=column  DO
  .  NEW trigname
  .  SET trigname=^%ydboctoocto("xref_status",tableName,column)
  .  ; Delete the trigger named `trigname` now that the cross reference is gone
  .  IF '$$dollarZTRIGGER^%ydboctoplanhelpers("ITEM","-"_trigname)  WRITE $ZSTATUS,!
  .  KILL ^%ydboctoocto("xref_status",tableName,column)
  QUIT
