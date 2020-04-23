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
; This module is derived from FIS GT.M.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note: This file is a copy of YDBTest repo 'com/difftime.m'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

difftime(x,y)	; diff the two times input ($H format) to return (in seconds) x-y
	new xpiece1,xpiece2,ypiece1,ypiece2,timediff
	set xpiece1=$piece(x,",")
	set xpiece2=$piece(x,",",2)
	set ypiece1=$piece(y,",")
	set ypiece2=$piece(y,",",2)
	if (xpiece1=ypiece1)  set timediff=xpiece2-ypiece2
	else  set timediff=(xpiece1-ypiece1)*24*60*60+(xpiece2-ypiece2)
	; Negative time is possible only when the local time gets switched from DST to normal,
	; exactly between noting down the start time and end time. Add 3600 to timediff
	if (timediff<0) do
	.	set timediff=timediff+3600
	; With normally small timeouts, an elapsed time of more than 3600 is not expected.
	; Possibly the local time was switched from normal to DST between noting down the start time and end time.
	; Subtract 3600 from timediff
	; But since this is possible that the timeout exceeded 1 hour and we would want to catch that,don't implement it
	quit (timediff)

addtime(x,y)	; add 'y' seconds (y can be positive or negative) to 'x' in $H format, return time in $H format
	new xpiece1,xpiece2,maxsec
	set xpiece1=$piece(x,",")
	set xpiece2=$piece(x,",",2)
	set xpiece2=xpiece2+y
	set maxsec=(24*60*60)
	if xpiece2<0 quit (xpiece1-1)_","_(xpiece2+maxsec)
	if xpiece2>(maxsec-1) quit (xpiece1+1)_","_(xpiece2-maxsec)
	quit xpiece1_","_xpiece2
