;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Below helper function is used by the auto_upgrade_binary_view_definition()
; -------------------------------------------------------------
ydboctoCreateViewsUpgradeNamesList
	; Input is of the following form and it represents a dependency graph
	;   ^%ydboctoocto("viewdependency","view_name","views","view_name")
	;   The first line below indicates that the view name "Z" depends on the view name "A". And so on.
	;     ^%ydboctoocto("viewdependency","Z","views","A")=""
	;     ^%ydboctoocto("viewdependency","Y","views","B")=""
	;     ^%ydboctoocto("viewdependency","Y","views","C")=""
	;     ^%ydboctoocto("viewdependency","Y","views","Z")=""
	; Given the above type of nodes this routine produces a sorted names list stored under %viewssortednames LVN. In this list
	; the dependent relation names show up AFTER the relation names they are dependent on. Views which do not have any
	; dependencies are also included in this list.
	; Following is the sorted names list for the example input shown above:
	;   %viewssortednames=5
	;   %viewssortednames(1)="A"
	;   %viewssortednames(2)="B"
	;   %viewssortednames(3)="C"
	;   %viewssortednames(4)="Z"
	;   %viewssortednames(5)="Y"
	KILL %viewssortednames
	new subs1,subs2,names,xdepends
	set subs1="" for  set subs1=$order(^%ydboctoocto("viewdependency",subs1))  quit:subs1=""  do
	. set names(subs1)=""
	. set subs2="" for  set subs2=$order(^%ydboctoocto("viewdependency",subs1,"views",subs2))  quit:subs2=""  do
	. . set:'$data(names(subs2)) names(subs2)=""
	. . set xdepends(subs2,subs1)=""
	. . set depends(subs1,subs2)=""
	for  do  quit:'$data(names)
	. set subs2="" for  set subs2=$order(names(subs2))  quit:subs2=""  do
	. . ; do:'$data(^%ydboctoocto("viewdependency",subs2,"views"))
	. . do:'$data(depends(subs2))
	. . . set %viewssortednames($incr(%viewssortednames))=subs2
	. . . set subs1="" for  set subs1=$order(xdepends(subs2,subs1))  quit:subs1=""  do
	. . . . ; kill ^%ydboctoocto("viewdependency",subs1,"views",subs2)
	. . . . kill depends(subs1,subs2)
	. . . kill names(subs2)
	QUIT
