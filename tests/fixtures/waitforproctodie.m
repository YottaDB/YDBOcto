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
waitforproctodie(pid,maxwait)
	;
	; Waits for "pid" to die and then returns
	; Waits by default for a max of 1 hour
	; Can set variable "maxwait" to a specific value (in seconds to wait) if the default max wait needs to be overridden
	;
	new alivepid
	if $data(maxwait)=0 set maxwait=3600 ; max. wait for 1 hour
	set alivepid=$$xwaitforproctodie(pid,maxwait)
	if alivepid'=""  do
	.	write "TEST-E-JOBWAITTOOLONG : Process id ",alivepid," alive after waiting for ",maxwait," seconds",!
	.	zshow "*"
	quit

xwaitforproctodie(pid,maxwait)
	; extrinsic version of waitforproctodie, returning the pid for timeout case
	new j,childalive
	if $data(maxwait)=0 set maxwait=3600 ; max. wait for 1 hour
	for j=1:1:maxwait  do  quit:childalive=0
	.	hang 1
	.	set childalive=$$^isprcalv(pid)
	quit $select(j'<maxwait:pid,1:"")

waitforalltodie(pids,maxwait)
	;
	; Waits for subscripts of "pids" to die and then returns
	; Waits by default for a max of 1 hour
	; Can set variable "maxwait" to a specific value (in seconds to wait) if the default max wait needs to be overridden
	; Must be called by reference, with a dot in front of the first argument.
	; e.g.
	;	kill mypids
	;	set mypids(1)=1234,mypids(2)=5678
	;	do waitforalltodie^waitforproctodie(.mypids,60)
	;
	new alivepids
	if $data(maxwait)=0 set maxwait=3600 ; max. wait for 1 hour
	set alivepids=$$xwaitforalltodie(.pids,maxwait)
	if alivepids'=""  do
	.	write "TEST-E-JOBWAITTOOLONG : Process id(s) (",waitpids,") alive after waiting for ",maxwait," seconds",!
	.	zshow "*"
	quit

xwaitforalltodie(pids,maxwait)
	; extrinsic version of waitforalltodie, returning comma delimited pids for timeout case
	new j,k,waitpids
	merge waitpids=pids
	if $data(maxwait)=0 set maxwait=3600 ; max. wait for 1 hour
	for j=1:1:maxwait  do  quit:$order(waitpids(""))=""
	.	hang 1
	.	set k=""  for  set k=$order(waitpids(k))  quit:k=""  kill:'$$^isprcalv(waitpids(k)) waitpids(k)
	set waitpids="",k=$order(waitpids(""))
	if k'=""  do
	.	; reuse waitpids root as string list of pid subscripts for output
	.	set waitpids=waitpids(k)
	.	for  set k=$order(waitpids(k))  quit:k=""  set waitpids=waitpids_","_waitpids(k)
	quit waitpids
