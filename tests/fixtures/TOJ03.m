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

; -----------------------------------------------------------------------------------------------------
; This program generates a random SQL query that optionally uses INNER or OUTER JOINs with a join nesting
; depth up to "$zcmdline" using the customers database. And runs this query against Postgres and Octo and verifies
; the outputs are identical. Certain features of this test are currently disabled due to pending issues.
; Those lines are marked with a ##TMPDISABLE.
; -----------------------------------------------------------------------------------------------------

genrandomqueries	;
	set maxjoins=+$zcmdline
	set primarykey("customers")="customer_id"
	set primarykey("orders")="order_id"
	; Define possible values of columns in customers and orders table (later used by ON and/or WHERE clause)
	set numcolumns("customers")=4
	set numcolumns("customers",0)="customer_id"
	set numcolumns("customers",1)="first_name"
	set numcolumns("customers",2)="last_name"
	set numcolumns("customers",3)="zipcode"
	set numcolumns("orders")=4
	set numcolumns("orders",0)="order_id"
	set numcolumns("orders",1)="order_date"
	set numcolumns("orders",2)="order_amount"
	set numcolumns("orders",3)="customer_id"
	;
	set numtype("customers","customer_id")=""	; indicates numeric; non-existence implies VARCHAR/CHAR
	set columns("customers","customer_id",1)=""
	set columns("customers","customer_id",2)=""
	set columns("customers","customer_id",3)=""
	set columns("customers","customer_id",4)=""
	set columns("customers","customer_id",5)=""
	set columns("customers","first_name","George")=""
	set columns("customers","first_name","John")=""
	set columns("customers","first_name","Thomas")=""
	set columns("customers","first_name","James")=""
	set columns("customers","last_name","Washington")=""
	set columns("customers","last_name","Adams")=""
	set columns("customers","last_name","Jefferson")=""
	set columns("customers","last_name","Madison")=""
	set columns("customers","last_name","Monroe")=""
	set columns("customers","zipcode","22121")=""
	set columns("customers","zipcode","02169")=""
	set columns("customers","zipcode","22902")=""
	set columns("customers","zipcode","22960")=""
	set numtype("orders","order_id")=""	; indicates numeric; non-existence implies VARCHAR/CHAR
	set columns("orders","order_id",1)=""
	set columns("orders","order_id",2)=""
	set columns("orders","order_id",3)=""
	set columns("orders","order_id",4)=""
	set columns("orders","order_id",5)=""
	set columns("orders","order_id",6)=""
	set columns("orders","order_date","07/04/1776")=""
	set columns("orders","order_date","03/14/1760")=""
	set columns("orders","order_date","05/23/1784")=""
	set columns("orders","order_date","09/03/1790")=""
	set columns("orders","order_date","07/21/1795")=""
	set columns("orders","order_date","11/27/1787")=""
	set columns("orders","order_amount","$234.56")=""
	set columns("orders","order_amount","$78.50")=""
	set columns("orders","order_amount","$124.00")=""
	set columns("orders","order_amount","$65.50")=""
	set columns("orders","order_amount","$25.50")=""
	set columns("orders","order_amount","$14.40")=""
	set numtype("orders","customer_id")=""	; indicates numeric; non-existence implies VARCHAR/CHAR
	set columns("orders","customer_id",1)=""
	set columns("orders","customer_id",2)=""
	set columns("orders","customer_id",3)=""
	set columns("orders","customer_id",9)=""
	set columns("orders","customer_id",10)=""
	do initnumvalues	; initialize # of possible values of each table/column combination
	;
	set joinstr(0)="inner join"
	set joinstr(1)="left join"
	set joinstr(2)="right join"
	set joinstr(3)="full join"
	set numqueries=30	; generate 30 queries so as not to take a long time for this test to run in pipeline
	set q=0
	for  do  quit:q=numqueries
	. set numjoins=1+(q#maxjoins)	; can be 1-way, 2-way, 3-way, ... up to n-way join where n is specified through $zcmdline
	. for i=1:1:numjoins  do
	. . set modulo=$random(2),table(i)=$select(modulo:"customers",1:"orders"),tablealias(i)=$extract(table(i),1)
	. ; choose table names for the join(s) next
	. set sqlquery="select "
	. ; choose select output column list
	. set numcols=0
	. for i=1:1:numjoins  do
	. . set modulo=$random(2)
	. . set:(i=numjoins)&('modulo) modulo=1
	. . quit:'modulo
	. . set:numcols sqlquery=sqlquery_","
	. . set column(numcols)=tablealias(i)_i_"."_primarykey(table(i))
	. . set sqlquery=sqlquery_column(numcols)  if $increment(numcols)
	. ; choose column names that are join candidates for each table
	. set i=1,sqlquery=sqlquery_" from "_table(i)_" "_tablealias(i)_i
	. set fulljoinchosen=0,notequalchosen=0
	. set outerjoinchosen=0
	. for i=2:1:numjoins  do
	. . set modulo=$random(4)
	. . set:(modulo>0) outerjoinchosen=1
	. . set sqlquery=sqlquery_" "_joinstr(modulo)_" "_table(i)_" "_tablealias(i)_i
	. . set:modulo>0 outerjoinchosen=1
	. . if modulo=3 do
	. . . ; Due to Postgres not supporting arbitrary ON clauses with a FULL JOIN, limit it to a simple one
	. . . set fulljoinchosen=1
	. . . set left=tablealias(i-1)_(i-1)_".customer_id"
	. . . set right=tablealias(i)_(i)_".customer_id"
	. . else  do
	. . . ; Not a FULL JOIN so can choose arbitrary ON clause
	. . . do chooseOnClauseOperands(.left,.right,i)
	. . set modulo=$random(2)
	. . set:modulo=1 notequalchosen=1
	. . set sqlquery=sqlquery_" on "_left_" "_$select(modulo:"!=",1:"=")_" "_right
	. ; Add optional WHERE
	. if $random(2) do
	. . set sqlquery=sqlquery_" where "_$$boolexpr(1+$random(4))
	. ; Add optional ORDER BY.
	. set orderby=$random(2)
	. if orderby do
	. . ; Since we are going to choose a list of columns in ORDER BY, we need to maintain a list of unmatched
	. . ; columns (compared against the SELECT column list) for later use when we need to see if an exact check
	. . ; can be done or not when LIMIT also gets randomly chosen in the query. Hence start with all columns being unmatched.
	. . merge unmatchedselectcolumn=column
	. . set sqlquery=sqlquery_" order by "
	. . set numorderbycols=1+$random(numcols)
	. . set sqlquery=sqlquery_column($$chooseorderbycolumn(numcols,.unmatchedselectcolumn))
	. . for i=2:1:numorderbycols set sqlquery=sqlquery_","_column($$chooseorderbycolumn(numcols,.unmatchedselectcolumn))
	. ; Add optional LIMIT
	. ; Note that in this case, the output order between Postgres and Octo could be different
	. ; and hence we cannot reliably get the test to pass.
	. ; Also can only do this if the # of columns selected for ORDER BY is the same as the # of SELECT columns
	. ; Or else the order of row output could differ in the columns that are not selected.
	. set nolimit=$random(4)
	. if 'nolimit do
	. . set sqlquery=sqlquery_" limit "_$random(10)
	. set sqlquery=sqlquery_";"
	. ; If LIMIT is not part of the query, then we can do an exact check of the results between Postgres and Octo
	. ; But if LIMIT is part of the query, then we have to check if ORDER BY is done on all the columns that are
	. ;	part of the SELECT column list. Even if one column in the SELECT column list is left out in the ORDER BY
	. ;	we cannot do an exact check since Postgres and Octo are free to present that column in an arbitrary order.
	. ;	Hence the check for existence of an unmatched select column using `$data(unmatchedselectcolumn)`
	. set outputsorted=(orderby&'$data(unmatchedselectcolumn))
	. ; The below if check is because postgres issues the following error if FULL JOIN and != in ON clause is chosen
	. ;	--> ERROR:  FULL JOIN is only supported with merge-joinable or hash-joinable join conditions
	. quit:fulljoinchosen&notequalchosen
	. set file="jointest"_$translate($justify($increment(q),2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write sqlquery
	. write:'outputsorted " ",$select('nolimit:"-- rowcount-only-check",1:"-- sort-needed-check")
	. write !
	. close file
	quit
	;
boolexpr(maxdepth)
	;
	new d,t,col,o,oper,value,ncol,colname,nchoice,k,subs,boolstr,delim,depth
	if maxdepth=0  do  quit boolstr
	. set t=1+$random(numjoins)
	. set col=tablealias(t)_t
	. set ncol=$random(numcolumns(table(t))),colname=numcolumns(table(t),ncol)
	. set col=col_"."_colname
	. set o=$random(8)
	. set oper=$select(o=0:"=",o=1:"<",o=2:">",o=3:$select($random(2):"!=",1:"<>"),o=4:"<=",o=5:">=",o=6:"IS NULL",o=7:"IS NOT NULL")
	. set value=""
	. if o<6 do
	. . set value=" "
	. . set nchoice=$random(5)
	. . set subs="" for k=1:1 set subs=$order(columns(table(t),colname,subs)) quit:(subs="")!(k>nchoice)
	. . set delim=$select($data(numtype(table(t),colname)):"",1:"'")
	. . set value=value_delim_subs_delim
	. set boolstr="("_col_" "_oper_value_")"
	set depth=1+$random(maxdepth)
	set boolstr=$$boolexpr(maxdepth-1)
	set oper=$select($random(2):"OR",1:"AND")
	for d=2:1:depth set boolstr=boolstr_" "_oper_" "_$$boolexpr(maxdepth-1)
	quit boolstr
	;
chooseOnClauseOperands(left,right,i)
	; left is LEFT operand of ON clause that we need to fill in
	; right is RIGHT operand of ON clause that we need to fill in
	; i is loop index indicating which join we are right now at (i can range from 2 to `numjoins`)
	;
	new rand
	set rand=$random(16)
	; Randomly (with 1/16 probability) choose both sides to be literals.
	if 0=rand do  quit
	. if $random(2) do
	. .	; choose literals that are not equal to each other
	. .	set left="1",right="2"
	. else  do
	. .	; choose literals that are equal to each other
	. .	set left="1",right="1"
	; Randomly (with 3/16 probability) choose one side to be a literal and one side to be a column reference.
	if 4>rand do  quit
	. ; choose column reference randomly from available tables till `i`th index
	. new t,col,ncol,colref,colname,lit,delim
	. set t=1+$random(i)
	. set col=tablealias(t)_t
	. set ncol=$random(numcolumns(table(t))),colname=numcolumns(table(t),ncol)
	. set colref=tablealias(t)_t_"."_colname
	. ; choose literal based on the chosen column reference
	. set lit=value(table(t),colname,1+$random(value(table(t),colname)))
	. set delim=$select($data(numtype(table(t),colname)):"",1:"'")
	. set lit=delim_lit_delim
	. ; choose literal position (in left or right) randomly
	. if $random(2) do
	. . set left=colref,right=lit
	. else  do
	. . set left=lit,right=colref
	; Randomly (with 8/16 probability) choose both sides to be column references.
	; Choose one column reference randomly from available tables till `i`th index.
	new t1,t2,col,ncol,colref1,colref2,colname
	set t1=1+$random(i)
	set col=tablealias(t1)_t1
	set ncol=$random(numcolumns(table(t1))),colname=numcolumns(table(t1),ncol)
	set colref1=tablealias(t1)_t1_"."_colname
	; Choose second column reference randomly from available tables till `i`th index.
	if "customer_id"=colname  do
	. ; If `customer_id` is chosen column in first table, then second table can be arbitrarily chosen as it is guaranteed
	. ; to have a `customer_id` column.
	. set t2=1+$random(i)
	. set col=tablealias(t2)_t2
	. set colref2=tablealias(t2)_t2_"."_"customer_id"
	else  do
	. ; Chosen column in first table is not `customer_id`. In that case, the same column has to be chosen in the
	. ; second table too. And that second table has to match the tablename of the first table even though its index
	. ; could be different (same table name could be chosen for multiple tables in the N-way join list).
	. new j,matchnum
	. for j=1:1:i if tablealias(j)=tablealias(t1) set matchnum($increment(matchnum))=j
	. set t2=1+$random(matchnum)
	. set colref2=tablealias(matchnum(t2))_matchnum(t2)_"."_colname
	; choose column reference position (in left or right) randomly
	if $random(2) do
	. set left=colref1,right=colref2
	else  do
	. set left=colref2,right=colref1
	quit

initnumvalues;
	new tablename,columnname,numvalues
	set tablename="" for  set tablename=$order(columns(tablename))  quit:tablename=""  do
	. set columnname="" for  set columnname=$order(columns(tablename,columnname))  quit:columnname=""  do
	. . set numvalues=0,value="" for  set value=$order(columns(tablename,columnname,value))  quit:value=""  do
	. . . if $increment(numvalues)
	. . . set value(tablename,columnname,numvalues)=value
	. . set value(tablename,columnname)=numvalues
	quit

chooseorderbycolumn(numcols,unmatchedselectcolumn)
	new col
	set col=$random(numcols)
	kill unmatchedselectcolumn(col)	; to indicate this column in the SELECT column list has been chosen/matched in the ORDER BY
	quit col
