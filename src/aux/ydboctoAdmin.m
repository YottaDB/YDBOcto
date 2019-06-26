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


; Handles the prompt for ydboctoAdmin cli usage
; prompt is in form "ydboctoAdmin action subAction args"
; @example
; ydboctoAdmin add user Arthur
; 	action: add; subAction: user; args: Arthur
;
; do all the printing in the prompt so the functions works as a silent api
;
ydboctoAdmin ;
	; ydboctoAdmin: Perform administrative tasks on YottaDB Octo database
	; Usage:
	;        ydboctoAdmin add user <name>
	;        ydboctoAdmin delete user <name>
	;        ydboctoAdmin show users
	; Options:
	;        -h --help	Print this message

	new action,subAction,user
	set action=$piece($zcmdline," ",1)
	set subAction=$piece($zcmdline," ",2)
	if "add"=action do
		. if "user"=subAction do
		. . if 3'=$length($zcmdline," ") do usage() quit
		. . set user=$piece($zcmdline," ",3)
		. . if ""'=$get(^%ydboctoocto("users",user)) write "AddUser: That user already exists",! quit
		. . write "Enter password for user ",user,": "
		. . use $principal:noecho ; disable echo here so plaintext password is not printed
		. . read rawPass
		. . use $principal:echo
		. . do addUser(user,rawPass)
		. . write !,"Successfully added user: """,user,"""",!
		. else  do usage()
	else  if "delete"=action do
		. if "user"=subAction do
		. . if 3'=$length($zcmdline," ") do usage() quit
		. . set user=$piece($zcmdline," ",3,$length($zcmdline," "))
		. . if ""=$get(^%ydboctoocto("users",user)) write "DeleteUser: That user does not exist",! quit
		. . do deleteUser(user)
		. . write "Successfully deleted user: """,user,"""",!
		. else  do usage()
	else  if "show"=action do
		. if "users"=subAction do
		. . do showUsers()
		. else  do usage()
	else  do usage()
	quit

; Prints usage for ydboctoAdmin
usage()
	new j,k,label,tmp
	set label=$text(+0)
	for j=1:1 set tmp=$piece($text(@label+j),"; ",2) quit:""=tmp  do
	. write $piece(tmp,"$text(+0)",1) for k=2:1:$length(tmp,"$text(+0)") write $text(+0),$piece(tmp,"$text(+0)",k)
	. write !
	quit

; Adds a user and md5 password hash to the database
;
; @param {string} user - username for the user
; @param {string} rawPass - the plaintext password for the user
;
; @example
; d addUser^ydboctoAdmin("Arthur","qwerty")
;
addUser(user,rawPass)
	new id,pass,ddlString
	set id=$incr(^%ydboctoocto("users"))
	set pass=$$MD5(rawPass_user)
	set $piece(ddlString,"|",12)=""
	set $piece(ddlString,"|",1)=id
	set $piece(ddlString,"|",2)=user
	set $piece(ddlString,"|",11)=pass
	set ^%ydboctoocto("users",user)=ddlString
	quit

; Adds a user deletes a user from the database
;
; @param {string} user - username for the user
;
; @example
; d deleteUser^ydboctoAdmin("Arthur")
;
deleteUser(user)
	kill ^%ydboctoocto("users",user)
	quit

; Prints the usernames for everyone in the database
; Sorted by id number
;
; @example
; d showUsers^ydboctoAdmin("Arthur")
;
showUsers()
	new user,users,i,row,id
	set user=$order(^%ydboctoocto("users",""))
	for i=0:1  quit:""=user  do
		. set row=^%ydboctoocto("users",user)
		. set id=$piece(row,"|",1)
		. set users(id)=user
		. set user=$order(^%ydboctoocto("users",user))
	if i=0 write "No YDBOcto users found.",! quit
	write "Current YDBOcto users, by ID:",!
	set id=$order(users(""))
	for  quit:""=id  write id,$char(9),users(id),! set id=$order(users(id))
	quit

%ydboctohash
;
; Compute various hashes used by Octo. This initial implementation
; uses external programs in PIPE devices. For future optimization
; for performance, modify to call library functions in-process
;
; Top level entry not supported
	do Etrap
	set $ecode=",U255,"	; top level entry not supported
	quit			; should never get here because previous line should terminate process

Etrap	; Set error handler to print error message and return error code to shell
	open "/proc/self/fd/2" ; open stderr for output if needed
	set $etrap="set $etrap=""use """"/proc/self/fd/2"""" write $zstatus,! zhalt 1"" set tmp1=$zpiece($ecode,"","",2),tmp2=$text(@tmp1) if $zlength(tmp2) use ""/proc/self/fd/2"" write $text(+0),@$zpiece(tmp2,"";"",2),! zhalt +$extract(tmp1,2,$zlength(tmp1))"
	quit

MD5(blob)
	; return hexadecimal string corresponding to MD5 hash of binary input byte sequence blob
	new hash,io
	set io=$io
	open "md5sum":(shell="/bin/sh":command="md5sum -b":stream:nowrap)::"pipe"
	use "md5sum"
	write blob
	set $x=0
	write /eof
	read hash
	use io close "md5sum"
	quit "md5"_$zpiece(hash," ",1)

;	Error message texts
U255	;"-F-BADINVOCATION Must invoke as yottadb -run set^"_$text(+0)_" or yottadb -run unset^"_$text(+0)
