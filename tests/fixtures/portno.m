;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	;
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
	new port,portisopen,portdir
	set portisopen=0
	for port=+$zcmdline:1 do  quit:portisopen
	.	; Check if port has been already allocated. If so, move on to next port.
	.	set portdir=$get(^portdir(port))
	.	if (""=$zsearch(portdir)) do
	.	.	do:(""'=portdir) scavenge
	.	.	; Port has not been allocated. Check if it is in use by any process in system currently.
	.	.	zsystem "cat /proc/net/tcp | grep -q `printf ""%04x"" "_port_"`"
	.	.	set portisopen=$zsystem
	set ^portdir(port)=$zdir
	write port
	quit

releaseport
	;
	; Release port
	;
	new port
	set port=+$zcmdline
	write:'$data(^portdir(port)) "TEST-E-RELEASEPORT : port "_port_" expected to be allocated but is not",!
	kill ^portdir(port)
	quit

scavenge
	new port
	set port="" for  set port=$order(^portdir(port)) quit:port=""  do
	.	kill:(""=$zsearch(^portdir(port))) ^portdir(port)
	quit

