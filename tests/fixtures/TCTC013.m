;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; YDBOcto#770
;
; This program generates a sequence of the following random SQL commands
; - CREATE TABLE that has randomly chosen number of columns and random set of PRIMARY KEY, UNIQUE and CHECK constraints
; - INSERT INTO
; - UPDATE
; - DELETE FROM
; - DROP TABLE
;
; It generates multiple sets of the above commands so one Octo process runs the same (we have previously seen some issues
; in octo running multiple sets, e.g. https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1098951468).
;
; The caller of this M program runs these commands against Postgres and Octo and verifies the outputs are identical.
; -----------------------------------------------------------------------------------------------------

octo770	;
	new ntables,ncols
	set maxcols=5,maxval=8
	for ntables=1:1:10 do
	. set ncols=1+$random(maxcols)
	. do create(ncols)
	. new j
	. for j=1:1:10 do command
	. do select(ncols);
	. write !
	do drop
	quit

command	;
	new rand
	set rand=$random(8)
	if rand=0 do select(ncols)
	else  if rand<3 do insert(ncols)
	else  if rand<6 do update(ncols)
	else  do delete(ncols)
	quit

select(ncols);
	new i
	write "select * from tmp order by "
	for i=1:1:ncols do
	. write "id"_i
	. write:(i'=ncols) ", "
	write ";",!
	quit

insert(ncols);
	new i
	write "insert into tmp values ("
	for i=1:1:ncols do
	. write $$literal
	. write:i'=ncols ", "
	write ");",!
	quit

update(ncols);
	new updcols,i
	for i=1:1:ncols if $random(2),$increment(updcols(i))
	quit:'$data(updcols)
	set updcols=0
	write "update tmp set "
	set i="" for  set i=$order(updcols(i),1)  quit:i=""  do
	. write:(0'=updcols) ", "
	. write "id"_i_" = "_$$arithexpr(ncols)
	. if $increment(updcols)
	; do full table update 25% of the time
	write:(0'=$random(4)) " where ",$$whereclause(ncols)
	write ";",!
	quit

arithexpr(ncols)
	new rand
	set rand=$random(2)
	quit:rand=0 $$literal
	new ret
	set ret=$$randcol(ncols)
	set rand=$random(2)
	set ret=ret_$select(rand=0:"+",1:"-")
	; We had the following M code previously.
	;	set ret=ret_$$literal
	;
	; An example set of commands that got generated in one rare case using the above M code is the following.
	;
	; drop table if exists tmp;
	; create table tmp (id1 integer, primary key (id1));
	; insert into tmp values (1);
	; insert into tmp values (4);
	; insert into tmp values (3);
	; update tmp set id1 = id1+1;
	;
	; The "update" command above results in a ERR_DUPLICATE_KEY_VALUE error in Octo but no error in Postgres.
	; This is due to Postgres updating rows in the order 1, 4, 3 (the order in which the rows were inserted)
	; whereas Octo does it in the order 1, 3, 4 (the order in which rows are stored in the global variable tree).
	; To avoid such rare failures, we add "maxval" to the generated literal.
	;
	; That is, the "update" command changed
	;	From: update tmp set id1 = id1+1;
	;	To  : update tmp set id1 = id1+(1+8);
	;
	; This is hoped to make it almost impossible to see such rare failures.
	;
	set ret=ret_($$literal+maxval)
	quit ret

delete(ncols);
	write "delete from tmp"
	; do full table delete 25% of the time
	write:(0'=$random(4)) " where ",$$whereclause(ncols)
	write ";",!
	quit

drop	;
	write "drop table if exists tmp;",!
	quit

create(ncols);
	new i
	do drop
	write "create table tmp ("
	for i=1:1:ncols do
	. ; TODO: Enhance column to be of numeric, string or boolean type too, not just integer type
	. write "id"_i_" integer"
	. write:(i'=ncols) ", "
	; Generate PRIMARY KEY constraint
	do primkey(ncols)
	; Generate UNIQUE constraint
	do unique(ncols)
	; Generate CHECK constraint
	do check(ncols)
	write ");",!
	quit

primkey(ncols)
	new primkeycols,result
	set result=$$randcols(ncols)
	write:(""'=result) ", primary key ("_result_")"
	quit

unique(ncols)
	new numuniq,i,result
	set numuniq=$random(3)
	for i=1:1:numuniq do
	. set result=$$randcols(ncols)
	. write:(""'=result) ", unique ("_result_")"
	quit

check(ncols)
	new numcheck,i
	set numcheck=$random(3)
	for i=1:1:numcheck do
	. write ", check ("
	. write $$randcol(ncols)," ",$$booleanrhs
	. write ")"
	quit

randcols(ncols)
	new colnum,result,i
	set result=""
	set colnum=0
	for i=1:1:ncols do
	. quit:$random(2)
	. set:(0'=colnum) result=result_", "
	. set result=result_"id"_i
	. if $increment(colnum)
	quit result

randcol(ncols)
	quit "id"_(1+$random(ncols))

booleanrhs()
	new rand,ret
	set rand=$random(2)
	quit:rand=0 $$oper_" "_$$literal
	; Test IN with list of literals
	set ret="in ("
	new i,max
	set max=1+$random(4)
	for i=1:1:max do
	. set ret=ret_$$literal
	. set:i'=max ret=ret_", "
	set ret=ret_")"
	quit ret

oper()
	new rand
	set rand=$random(4)
	quit $select(rand=0:"=",rand=1:"!=",rand=2:"<",1:">")

literal()
	; TODO: Enhance literal to also be numeric, string or boolean type too, not just integer type
	quit $random(maxval)

boolexpr()
	quit "("_$$randcol(ncols)_" "_$$booleanrhs_")"

whereclause(ncols)
	new numopers
	set numopers=1+$random(3)
	new i,ret
	set ret=""
	for i=1:1:numopers do
	. set ret=ret_$$boolexpr
	. set:(i'=numopers) ret=ret_" "_$select($random(2)=0:"or",1:"and")_" "
	quit ret

