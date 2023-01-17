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

; -----------------------------------------------------------------------------------------------------
; This program helps test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/509#note_1237785877.
; It is described as "test.m" there. The caller is supposed to invoke this from multiple concurrent
; processes and pipe the output of this program (CREATE TABLE and DROP TABLE commands) to Octo.
; -----------------------------------------------------------------------------------------------------

TDRC05	;
	quit

gensql	;
	for i=1:1:10 do
	. for j=1:1:4 do
	. . write "drop table xxx"_j_";",!
	. . write "create table xxx"_j_"(id1 integer, id2 integer, PRIMARY KEY (id1, id2));",!
	. . write "insert into xxx"_j_" values ("_$j_",",j,");",!
	. . write "select * from xxx"_j_";",!
	quit

job	;
	; We expect a few errors (e.g. ERR_CANNOT_CREATE_TABLE, ERR_CANNOT_DROP_TABLE etc.).
	; So we do not want the job framework to catch these. Hence the set of jnoerrchk to 1 below.
	set jnoerrchk=1
	do ^job("child^TDRC05",8,"""""")
	quit

child	;
	new starttime,inputfile
	set starttime=$horolog
	set inputfile="TDRC05.sql"
	for  do  quit:$$^difftime($horolog,starttime)>2
	. write "# ",$zut," : Running : octo -f ",inputfile,!
	. zsystem "octo -f "_inputfile
	. if $zsystem write "octo -f "_inputfile_" : failed with exit status "_$zsystem,!
	quit

