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
; Checks if "pid" passed as input is alive or not
;
; Returns
;	--> 1 if "pid" is alive
;	--> 0 if "pid" is not alive
;
isprcalv(pid);
	new $ztrap,alive
	set $ztrap="goto errjpi"
	set alive=$zgetjpi(pid,"ISPROCALIVE")
	quit alive
isalive;
	set unix=$ZVersion'["VMS"
	if unix do
	.	set alive=0
	.	set zsystr="ps -p "_pid_" >& /dev/null"
	.	zsystem zsystr
	.	if $zsystem=0 set alive=1
	if 'unix do
	.	set alive=-1
	.	set zsystr="pipe show process /id="_$$FUNC^%DH(pid)
	.	set nooutput=" >nl: 2>nl:"
	.	zsystem zsystr_nooutput
	.	; in VMS, the last 4 hexadecimal bytes of the status should be 0x0001 to be considered success
	.	set status=$$FUNC^%DH($zsystem)
	.	if status="100008E8" set alive=0	; %SYSTEM-W-NONEXPR, nonexistent process
	.	if status="100003A4" set alive=1	; %SYSTEM-F-SUSPENDED, process is suspended
	.	if status="10000001" set alive=1	; Normal status (indicates existent process)
	.	if alive=-1  do				; Indicates error in show process/id
	.	.	write "ISPRCALV-E-SHOWPROC : status = ",status,!
	.	.	write "The output of ZSYSTEM ",zsystr," follows ",!
	.	.	zsystem zsystr	; print output on screen
	.	.	set alive=1	; treat this as if the process still exists
	quit
errjpi;
	; This is taken from merge/inref/errcont.m. We may want to have a generic routine to continue after an error
	new savestat,mystat,prog,line,newprog
	set savestat=$zstatus
	set mystat=$piece(savestat,",",2,100)
	set prog=$piece($zstatus,",",2,2)
	set line=$piece($piece(prog,"+",2,2),"^",1,1)
	set line=line+1
	set newprog=$piece(prog,"+",1)_"+"_line_"^"_$piece(prog,"^",2,3)
	set newprog=$zlevel_":"_newprog
	;
	if $ZSTATUS'["%YDB-E-BADJPIPARAM" halt
	do isalive
	ZGOTO @newprog
