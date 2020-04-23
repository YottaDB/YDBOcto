;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TC013	;
	quit

init	;
	; Create ddl.sql file that has multiple CREATE TABLE commands and SELECT commands for a table named LOTSOFCOLS.
	; The number of columns in the table keeps changing across the different CREATE TABLE commands but the underlying
	; M global ^lotsofcols does not change. This is later run concurrently with multiple processes along with SELECT
	; to make sure there are no issues with CREATE TABLE and SELECT commands running concurrently. Previously we used
	; to see an incorrect "Unknown table" error or assertion failure in find_table.c.
	;
	; Note: Some of this logic is similar to that in `tests/fixtures/TSC20.m`.
	;
	new ncols,file,i,j,logncols
	set logncols=(3+$random(8))	; ncols is randomly set to 8,16,32,64,...,1024
	set ^logncols=logncols		; needed later by verify^TC013
	set file="ddl.sql"
	open file:(newversion)
	use file
	for i=1:1:logncols do
	. ; write CREATE TABLE command with 2**i columns
	. write "create table lotsofcols (id INTEGER PRIMARY KEY,",!
	. set ncols=2**i
	. for j=1:1:(2**i) write "  col"_j_" VARCHAR"  write:j'=ncols "," write !
	. write ") GLOBAL ""^lotsofcols(keys(""""id""""))"";",!
	close file
	;
	; Create query to check the pg_catalog and determine all columns corresponding to LOTSOFCOLS table
	;
	set file="catalog.sql"
	open file:(newversion)
	use file
	write "select c.relname, a.attname",!
        write "from pg_catalog.pg_class as c",!
        write "inner join pg_catalog.pg_attribute as a on a.attrelid = c.oid",!
        write "where c.relname = 'LOTSOFCOLS';",!
	close file
	;
	; Create query to select all rows from the LOTSOFCOLS table
	;
	set file="select.sql"
	open file:(newversion)
	use file
	write "select * from lotsofcols;"
	close file
	;
	;
	; Populate ^lotsofcols global so it returns a few rows
	set val=""
	for j=1:1:ncols set val=val_j  set:j<ncols val=val_"|"
	for i=1:1:10 set ^lotsofcols(i)=val
	quit

job	;
	do ^job("child^TC013",8,"""""")
	quit

child	;
	new starttime,inputfile
	set starttime=$horolog
	set inputfile=$select(jobindex<4:"ddl.sql",jobindex<6:"catalog.sql",1:"select.sql")
	for  do  quit:$$^difftime($horolog,starttime)>10
	. write "# ",$zut," : Running : octo -f ",inputfile,!
	. zsystem "octo -f "_inputfile
	. if $zsystem write "octo -f "_inputfile_" : failed with exit status "_$zsystem,!
	quit

verify	;
	new i,file,mjofile
	for i=1:1:8 do
	. set mjofile="child_TC013.mjo"_i
	. open mjofile:(readonly)
	. use mjofile
	. new line,nlines
	. for  read line($increment(nlines)) quit:$zeof
	. close mjofile
	. kill line(nlines) if $increment(nlines,-1)
	. if i<4 do verifyddl(mjofile,.line,nlines)
	. else  do
	. . if i<6 do verifycatalog(mjofile,.line,nlines)
	. . else   do verifyselect(mjofile,.line,nlines)
	quit

verifyddl(mjofile,line,nlines)
	new i
	; The first 3 lines of output are from tests/fixtures/job.m so ignore them
	for i=4:1:nlines  do
	. quit:""=line(i)
	. if line(i)'["Running : octo -f ddl.sql" write "Verify failed",! zshow "*" halt
	quit

verifycatalog(mjofile,line,nlines)
	new i,j,k,match,logncols
	set logncols=^logncols
	; The first 3 lines of output are from tests/fixtures/job.m so ignore them
	for i=4:1:nlines  do
	. quit:""=line(i)
	. if line(i)'["Running : octo -f catalog.sql" write "Verify failed",! zshow "*" halt
	. if line($increment(i))'="LOTSOFCOLS|ID" write "Verify failed",! zshow "*" halt
	. ; Verify that output is of the form "LOTSOFCOLS|COLx" where x ranges from 1 thru 2**n followed by a blank line
	. for j=i+1:1  quit:""=line(j)  do
	. . if line(j)'=("LOTSOFCOLS|COL"_(j-i)) write "Verify failed",! zshow "*" halt
	. set match=0
	. for k=1:1:logncols set match=((2**k)=(j-1-i)) quit:match
	. if match=0 write "Verify failed",! zshow "*" halt
	. set i=j
	quit

verifyselect(mjofile,line,nlines)
	new i,j,k,match,logncols,ncols,expect
	set logncols=^logncols
	; The first 3 lines of output are from tests/fixtures/job.m so ignore them
	for i=4:1:nlines  do
	. quit:""=line(i)
	. if line(i)'["Running : octo -f select.sql" write "Verify failed",! zshow "*" halt
	. if $increment(i)
	. ; Determine the number of columns that got selected by examining the first line.
	. set ncols=$length(line(i),"|")-1
	. set match=0
	. for k=1:1:logncols set match=((2**k)=ncols) quit:match
	. if match=0 write "Verify failed",! zshow "*" halt
	. ; Extract all columns except id which changes for every row
	. set expect=$piece(line(i),"|",2,16384)
	. ; Verify that output is of the form "id|.|.|...|." where the # of dots in each line matches "ncols"
	. for j=i:1:i+9  do
	. . if line(j)'=((j-i+1)_"|"_expect) write "Verify failed",! zshow "*" halt
	. set i=j
	quit
