;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	;
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
	; Delete all _ydboctoP*.m and _ydboctoX*.m plans associated with "tableName".
	; Also delete those database nodes that correspond to the plan metadata of these deleted plans.
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
	.  IF '$$dollarZTRIGGER^%ydboctoplanhelpers("ITEM","-"_trigname) WRITE "Removing Trigger failed",!
	.  KILL ^%ydboctoocto("xref_status",tableName,column)
	; Remove AIM data (xref and triggers) stored in the AIM global
	DO
	.  NEW aimgbl
	.  SET column="" FOR  SET column=$ORDER(^%ydbAIMOctoCache(tableName,column))  QUIT:""=column  DO
	. .  SET aimgbl=$QSUBSCRIPT(^%ydbAIMOctoCache(tableName,column,"location"),0)
	. .  DO UNXREFDATA^%YDBAIM(aimgbl)
	. .  KILL ^%ydbAIMOctoCache(tableName,column)
	IF $data(tableGVNAME)&(""'=tableGVNAME) DO
	. ; If tableGVNAME is not "", it points to an gvn whose subtree needs to be KILLed as part of the DROP TABLE
	. KILL @tableGVNAME
	. ; Now that we know we are in the middle of a DROP TABLE and this table is going away, remove global variable nodes
	. ; that record which functions this table's CHECK constraints depend on.
	. NEW gvn
	. SET gvn="^%ydboctoocto(""tableconstraint"",tableName)"
	. FOR  SET gvn=$query(@gvn)  QUIT:$QSUBSCRIPT(gvn,2)'=tableName  DO
	. . ; gvn would be like ^%ydboctoocto("tableconstraint","NAMES","NAME1","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G")=""
	. . NEW constraintName,functionName,functionHash
	. . SET constraintName=$QSUBSCRIPT(gvn,3)
	. . SET functionName=$QSUBSCRIPT(gvn,4)
	. . SET functionHash=$QSUBSCRIPT(gvn,5)
	. . KILL @gvn
	. . ; Need to also kill the following gvn which is maintained in sync with the above
	. . ; ^%ydboctoocto("functions","SAMEVALUE","%ydboctoFN0uUSDY6E7G9VcjaOGNP9G","check_constraint","NAMES","NAME1")=""
	. . KILL ^%ydboctoocto("functions",functionName,functionHash,"check_constraint",tableName,constraintName)
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

