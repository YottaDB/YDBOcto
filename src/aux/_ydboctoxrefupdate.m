;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

handleSetKill(variable,value)
    ; M routine that is invoked to keep xref for SETTING column in PG_SETTINGS table in sync
    ; Note that this routine is called directly by plans modifying the LVN this table is stored in,
    ; rather than using a trigger. This is needed because triggers are only applicable to GVNs.
    NEW pgsettings
    IF $DATA(%ydboctoocto("settings","pg_settings",variable)) DO
    . ; The node existed previously so kill cross reference corresponding to its old value
    . NEW oldValue
    . SET oldValue=$PIECE(%ydboctoocto("settings","pg_settings",variable),"|",1)
    . DO delXrefEntry(variable,oldValue)
    DO addXrefEntry(variable,value)
    QUIT

addXrefEntry(variable,value)
    SET %ydboctoxref("PG_SETTINGS","SETTING",value,variable)=""
    IF $INCREMENT(%ydboctoxref("PG_SETTINGS","SETTING",value))
    IF $INCREMENT(%ydboctoxref("PG_SETTINGS","SETTING"))
    QUIT

delXrefEntry(variable,value)
    KILL %ydboctoxref("PG_SETTINGS","SETTING",value,variable)
    KILL:0=$INCREMENT(%ydboctoxref("PG_SETTINGS","SETTING",value),-1) %ydboctoxref("PG_SETTINGS","SETTING",value)
    KILL:0=$INCREMENT(%ydboctoxref("PG_SETTINGS","SETTING"),-1) %ydboctoxref("PG_SETTINGS","SETTING")
    QUIT

