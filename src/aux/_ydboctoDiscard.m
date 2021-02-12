;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Below helper functions are used by the DISCARD command
; Currently only DISCARD ALL is supported. But in the future
; DISCARD XREFS and DISCARD PLANS would be supported
; -------------------------------------------------------------

%ydboctoDiscard	;
	QUIT

discardALL	;
	; Discard plans, xrefs and triggers for ALL tables
	NEW tableName
	; Go through list of all tables
	SET tableName="" FOR  SET tableName=$ORDER(^%ydboctoschema(tableName))  QUIT:""=tableName  DO
	.  ; For each table discard plan/xref/triggers
	.  DO discardTable(tableName)
	; At this point all derived data should have been removed but it is possible some still remain
	; in case of application integrity error situation (e.g. for some tableName say TABLE, ^%ydboctoschema(TABLE)
	; does not exist but ^%ydboctoocto("xref_status",TABLE,*) does exist. Since we know we have to discard
	; everything, we just KILL all such possible out-of-sync global nodes and delete all known plans etc.
	NEW srcPlan
	SET srcPlan="" FOR  SET srcPlan=$ORDER(^%ydboctoocto("plandirs",srcPlan))  QUIT:""=srcPlan  DO discardPlan(srcPlan)
	KILL ^%ydboctoocto("plan_metadata")
	KILL ^%ydboctoocto("tableplans")
	KILL ^%ydboctoocto("xref_status")
	KILL ^%ydboctoxref
	QUIT

discardPLANS	;
	; Not currently implemented
	QUIT

discardXREFS	;
	; Not currently implemented
	QUIT

discardTable(tableName,tableGVNAME)	;
	; ----------------------------------------------------------------------------
	; Discards all generated xrefs, plans and triggers associated with a table.
	; Also KILLs the M global name associated with a table in case "tableGVNAME" parameter is set to a non-empty string.
	; ----------------------------------------------------------------------------
	; Delete all plans associated with "tableName"
	;
	; Ensure all _ydboctoP*.m plans that rely on this table are recreated by deleting those database nodes
	; that correspond to the plan metadata of these plans. We do not delete the _ydboctoP*.m files since
	; the _ydboctoP*.o files would anyways exist and also need to be removed but it is not straightforward (since
	; we need to find the first obj directory in the zroutines list). Just deleting the database nodes is enough
	; since that is checked every time using the GET_PLAN_METADATA_DB_NODE macro before using a pre-existing plan.
	;
	NEW planName
	SET planName="" FOR  SET planName=$ORDER(^%ydboctoocto("tableplans",tableName,planName))  QUIT:""=planName  DO
	.  DO discardPlan(planName)
	KILL ^%ydboctoocto("tableplans",tableName)
	; Delete all generated cross references associated with columns in "tableName".
	KILL ^%ydboctoxref(tableName)
	; Delete all generated triggers associated with columns in the table being created/deleted.
	; Delete all M global nodes in ^%ydboctoocto/^%ydboctoschema that point to the above deleted objects.
	NEW column
	SET column="" FOR  SET column=$ORDER(^%ydboctoocto("xref_status",tableName,column))  QUIT:""=column  DO
	.  NEW trigname
	.  SET trigname=^%ydboctoocto("xref_status",tableName,column)
	.  ; Delete the trigger named `trigname` now that the cross reference is gone
	.  IF '$$dollarZTRIGGER^%ydboctoplanhelpers("ITEM","-"_trigname)  WRITE $ZSTATUS,!
	.  KILL ^%ydboctoocto("xref_status",tableName,column)
	; If tableGVNAME is not "", it points to an gvn whose subtree needs to be KILLed as part of the DROP TABLE
	KILL:$data(tableGVNAME)&(""'=tableGVNAME) @tableGVNAME
	QUIT

discardFunction(functionName,functionHash)	;
	; --------------------------------------------------------------------------------------------------
	; Discards all generated plans associated with a function whose name and hash are provided as input.
	; --------------------------------------------------------------------------------------------------
	NEW planName
	SET planName="" FOR  DO  QUIT:""=planName  DO discardPlan(planName)
	.  SET planName=$ORDER(^%ydboctoocto("functions",functionName,functionHash,"plan_metadata",planName))
	KILL ^%ydboctoocto("functions",functionName,functionHash);
	QUIT

discardPlan(srcPlan)
	; ---------------------------------------------------------------------------------------------------------------
	; Internal helper function used by $$discardTable() and $$discardFunction()
	; Given a plan name (corresponding to an M file), it
	;   1) Deletes the .m file.
	;   2) Deletes the corresponding .o file.
	;   3) Deletes the M nodes corresponding to the plan.
	; Thereby ensuring all plan artifacts are erased and recreated the next time a need arises.
	; ---------------------------------------------------------------------------------------------------------------
	;
	; 1) Delete the .m file.
	DO deleteFile(srcPlan)
	;
	; 2) Delete the corresponding .o file (possible more than one .o file across multiple directories)
	NEW objPlan
	SET objPlan="" FOR  SET objPlan=$order(^%ydboctoocto("plandirs",srcPlan,objPlan)) QUIT:""=objPlan  DO
	. DO deleteFile(objPlan)
	KILL ^%ydboctoocto("plandirs",srcPlan)
	;
	; 3) Delete the M nodes corresponding to the plan
	KILL ^%ydboctoocto("plan_metadata",srcPlan)
	QUIT

deleteFile(fileName)
	; Deletes the input file.
	; If fileName does not exist, this function will create a file first (in the OPEN command below)
	; and then delete it (in the CLOSE command below). If there are errors in any of the commands,
	; we will skip error handling and return.
	; It is possible we do not have permissions to delete the file. In that
	;    case, skip this and move on to the next step. Hence the "exception" string.
	;    Set $ECODE to empty string in case of an exception just in case it causes error rethrow at caller M frame.
	OPEN fileName:(exception="set $ecode="""" goto deleteFileDone")
	CLOSE fileName:delete
deleteFileDone
	QUIT

