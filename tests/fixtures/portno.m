;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

portno	;
	quit

findopenport
	;
	; Finds an port that has not yet been allocated for Octo bats tests AND
	;	is not currently in use by any other process in the system
	;
	new port,portisopen,portdir,shellset
	set portisopen=0,shellset=0
	for port=+$zcmdline:1 do  quit:portisopen
	.	; Check if port has been already allocated. If so, move on to next port.
	.	set portdir=$get(^portdir(port))
	.	if (""=$zsearch(portdir)) do
	.	.	do:(""'=portdir) scavenge
	.	.	; Port has not been allocated. Check if it is in use by any process in system currently.
	.	.	; The "nc -z" command below checks if there is anything listening on the port.
	.	.	; We have noticed that if SHELL env var is set to "tcsh", then the exit status for a command
	.	.	; that was not found is 1 whereas in "bash", the exit status for the same is 127.
	.	.	; Since an exit status of 1 from "nc" also indicates NO process is listening, we do not want to
	.	.	; get a status of 1 when "nc" executable is not found (or for any other error scenario that nc
	.	.	; encounters even if the executable is present). Therefore we invoke zsystem with SHELL env var
	.	.	; set to "bash" to ensure $zsystem is properly set to a value other than 0 or 1 in case of an error.
	.	.	if 'shellset view "setenv":"SHELL":"/bin/bash" set shellset=1	; enough to do "setenv" once in process
	.	.	zsystem "nc -z localhost "_port
	.	.	set portisopen=$zsystem
	.	.	; "nc -z" returns 0 to indicate some process is listening.
	.	.	; "nc -z" returns 1 to indicate NO   process is listening.
	.	.	; Any other return value indicates an error so exit the M program in that case.
	.	.	; It is not easy to signal this error scenario from the M program to its caller bash script so
	.	.	; we set the "port" variable to a string that contains the error message. The caller will encounter
	.	.	; an error while trying to start rocto with such a non-numeric port string. And will fail the test
	.	.	; thereby achieving our objective.
	.	.	do:(portisopen'=0)&(portisopen'=1)
	.	.	.	set port="TEST-E-FAIL : nc -z localhost "_port_" exited with ["_portisopen_"] exit status"
	set ^portdir(port)=$zdir
	set ^history($zut,port,"findopenport",$zdir)=$ztrnlnm("BATS_TEST_NAME")
	write port
	quit

releaseport
	;
	; Release port
	;
	new port
	set port=+$zcmdline
	write:'$data(^portdir(port)) "TEST-E-RELEASEPORT : port "_port_" expected to be allocated but is not",!
	set ^history($zut,port,"releaseport",$zdir)=$ztrnlnm("BATS_TEST_NAME")
	kill ^portdir(port)
	quit

scavenge
	new port
	set port="" for  set port=$order(^portdir(port)) quit:port=""  do
	.	set ^history($zut,port,"scavenge",$zdir)=$ztrnlnm("BATS_TEST_NAME")
	.	kill:(""=$zsearch(^portdir(port))) ^portdir(port)
	quit

