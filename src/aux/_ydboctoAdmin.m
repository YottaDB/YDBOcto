;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Handles the prompt for _ydboctoAdmin cli usage
; prompt is in form "_ydboctoAdmin action subAction args"
; @example
; _ydboctoAdmin add user Arthur
; 	action: add; subAction: user; args: Arthur
;
; do all the printing in the prompt so the functions works as a silent api
;
%ydboctoAdmin ;
	; _ydboctoAdmin: Perform administrative tasks on YottaDB Octo database
	; Usage:
	;        _ydboctoAdmin add user <name>
	;        _ydboctoAdmin delete user <name>
	;        _ydboctoAdmin show users
	; Options:
	;        -a --allowschemachanges	When used with `add user`, grants the new user permission to run CREATE, ADD, and
	;					DROP statements. Automatically applies -w/--readwrite.
	;        -h --help			Print this message
	;        -w --readwrite			When used with `add user`, grants the new user permission to run INSERT, UPDATE, and
	;					DELETE statements.

	new action,subAction,user
	set action=$piece($zcmdline," ",1)
	set subAction=$piece($zcmdline," ",2)
	if "add"=action do
		. if "user"=subAction do
		. . if 3>$length($zcmdline," ") do usage() quit
		. . set user=$piece($zcmdline," ",3)
		. . if "-"=$extract(user) write "AddUser: Usernames cannot begin with '-' or '--'",! zhalt 1
		. . if 64<$length(user) write "AddUser: Usernames must be 64 characters or fewer in length",! zhalt 1
		. . if ""'=$get(^%ydboctoocto("users",user)) write "AddUser: That user already exists",! zhalt 1
		. . set status=$$addUser(user)
		. . zhalt:status status
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

; Prints usage for _ydboctoAdmin
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
;
; @example
; d addUser^%ydboctoAdmin("Arthur")
;
addUser(user)
	new options,option,quitonerror,permissions,permstr,allowschemachanges
	new id,rawPass1,rawPass2,pass,ddlString

	; Determine permissions for the new user.
	; Integer values indicate permissions level as specified in the UserPermissions_t enum in rocto.h.
	; The default is the most-restrictive setting, UserPermissions_ReadOnly
	set permissions=0 ; UserPermissions_ReadOnly
	set permstr="readonly"
	set quitonerror=0
	set allowschemachanges=0
	set options=$piece($zcmdline," ",4,$length($zcmdline," "))
	for i=1:1:$length(options," ")  do
	. set option=$piece(options," ",i)
	. if ("-"=$extract(option)) do
	. . if ("-a"=option)!("--allowschemachanges"=option) do
	. . . set allowschemachanges=1
	. . else  if ("-w"=option)!("--readwrite"=option) do
	. . . set permissions=1 ; UserPermissions_ReadWrite
	. . . set permstr="readwrite"
	. . else  do
	. . . set quitonerror=1
	. . . write "AddUser: Invalid option: '"_option_"'",!
	quit:quitonerror 1

	; Read user password after validating options to avoid needless hash if there is an invalid option
	write "Enter password for user ",user,": "
	use $principal:noecho ; disable echo here so plaintext password is not printed
	read rawPass1
	use $principal:echo
	write !,"Re-enter password for user ",user,": "
	use $principal:noecho ; disable echo here so plaintext password is not printed
	read rawPass2
	use $principal:echo
	if rawPass1'=rawPass2 do
	. write !,"Passwords don't match. Cancelling user creation.",!
	. set quitonerror=1
	quit:quitonerror 1
	set id=$incr(^%ydboctoocto("users"))
	set pass=$$MD5(rawPass1_user)
	set $piece(ddlString,"|",12)=""
	set $piece(ddlString,"|",1)=id
	set $piece(ddlString,"|",2)=user
	set $piece(ddlString,"|",11)=pass
	set ^%ydboctoocto("users",user)=ddlString

	; If allowschemachanges is set, increment the permissions setting (0 for RO and 1 for RW) by 2 to bring the value into
	; alignment with either UserPermissions_ROAllowSchemaChanges (read-only + allowschemachanges) or
	; UserPermissions_RWAllowSchemaChanges (read-write + allowschemachanges).
	; These values reflect those defined in the UserPermissions_t enum in rocto.h.
	do:allowschemachanges
	. set permissions=permissions+2
	. set permstr=permstr_"+allowschemachanges"
	; Set the final permissions value for the given user
	set ^%ydboctoocto("users",user,"permissions")=permissions
	write !,"Successfully added user: """,user,""" with """,permstr,""" permissions",!
	quit 0

; Deletes a user from the database
;
; @param {string} user - username for the user
;
; @example
; d deleteUser^%ydboctoAdmin("Arthur")
;
deleteUser(user)
	kill ^%ydboctoocto("users",user)
	quit

; Prints the usernames for everyone in the database
; Sorted by id number
;
; @example
; d showUsers^%ydboctoAdmin("Arthur")
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
