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

; -----------------------------------------------------------------------------------------------------
; This program generates a random SQL query that optionally uses INNER or OUTER JOINs with a join nesting
; depth up to 7 using the customers database. And runs this query against Postgres and Octo and verifies
; the outputs are identical. Certain features of this test are currently disabled due to pending issues.
; Those lines are marked with a ###TMPDISABLE.
; -----------------------------------------------------------------------------------------------------

genrandomqueries	;
	set primarykey("customers")="customer_id"
	set primarykey("orders")="order_id"
	; Define possible values of columns in customers and orders table (later used by WHERE clause)
	; Might be used by ON clause in the future too when #311 is fixed ###TMPDISABLE
	set numcolumns("customers")=4
	set numcolumns("customers",0)="customer_id"
	set numcolumns("customers",1)="first_name"
	set numcolumns("customers",2)="last_name"
	set numcolumns("customers",3)="zipcode"
	set numcolumns("orders")=2
	set numcolumns("orders",0)="order_date"
	set numcolumns("orders",1)="order_amount"
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
	;
	set joinstr(0)="inner join"
	set joinstr(1)="left join"
	set joinstr(2)="right join"
	set joinstr(3)="full join"
	set numqueries=30	; generate 30 queries so as not to take a long time for this test to run in pipeline
	set q=0
	for  do  quit:q=numqueries
	. set numjoins=1+(q#8)	; can be 1-way, 2-way, 3-way, ... up to 7-way join
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
	. . set sqlquery=sqlquery_column(numcols)  if $incr(numcols)
	. ; choose column names that are join candidates for each table
	. set i=1,sqlquery=sqlquery_" from "_table(i)_" "_tablealias(i)_i
	. set fulljoinchosen=0,notequalchosen=0	;  ###TMPDISABLE (until FULL JOINs work AND != check in LEFT JOINs work)
	. for i=2:1:numjoins  do
	. . set modulo=$random(4)
	. . set sqlquery=sqlquery_" "_joinstr(modulo)_" "_table(i)_" "_tablealias(i)_i
	. . set:modulo=3 fulljoinchosen=1
	. . set modulo=$random(2)
	. . set sqlquery=sqlquery_" on "_tablealias(i-1)_(i-1)_".customer_id "_$select(modulo:"!",1:"")
	. . set:modulo=1 notequalchosen=1
	. . set sqlquery=sqlquery_"= "_tablealias(i)_(i)_".customer_id"
	. ; Add optional WHERE
	. if (1=numjoins)&$random(2) do	 ; ###TMPDISABLE Remove (1=numjoins) once #311 is fixed
	. . set sqlquery=sqlquery_" where "_$$boolexpr(1+$random(4))
	. ; Add optional ORDER BY
	. set orderby=$random(2)
	. if orderby do
	. . ; Currently choose only ONE column for ORDER BY. When #228 is fixed, enable multiple columns here ###TMPDISABLE
	. . set sqlquery=sqlquery_" order by "_column($random(numcols))
	. ; Add optional LIMIT
	. ; Note that in this case, the output order between Postgres and Octo could be different
	. ; and hence we cannot reliably get the test to pass.
	. ; Also can only do this if the # of columns selected for ORDER BY is the same as the # of SELECT columns
	. ; Or else the order of row output could differ in the columns that are not selected.
	. set limit=$random(2)
	. if limit do
	. . set sqlquery=sqlquery_" limit "_$random(10)
	. set sqlquery=sqlquery_";"
	. set exactcheck=('limit)!(orderby&(1=numcols)) ; ###TMPDISABLE : change 1=numcols to numorderbycols=numcols when #228 is fixed
	. ; The below if check is because postgres issues the following error in this case
	. ;	--> ERROR:  FULL JOIN is only supported with merge-joinable or hash-joinable join conditions
	. quit:fulljoinchosen&notequalchosen
	. quit:notequalchosen			; ###TMPDISABLE until #311 is fixed
	. set file="jointest"_$translate($justify($incr(q),2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write:'exactcheck "-- rowcount-only-check",!
	. write sqlquery,!
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
genouterjoinonpastas;
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastas.id;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastas.id asc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastas.id desc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by lastName;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by lastName asc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by lastName desc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by firstName;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by firstName asc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by firstName desc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastaName;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastaName asc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta order by pastaName desc;"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where lastName = 'Buttons';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where firstName = 'Zero';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where firstName = 'Zero' and names4.favoritePasta = 'Lasagna';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where names4.favoritePasta = 'Penne';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where names4.favoritePasta = 'Spaghetti';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where names4.favoritePasta = 'Cavatelli';"
	set query($incr(query))="select distinct pastas.id, favoritePasta from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where names4.favoritePasta = 'Cavatelli';"
	set query($incr(query))="select pastas.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where lastName = 'Buttons' order by firstName;"
	set query($incr(query))="select names4.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where firstName = 'Zero' order by names4.id;"
	set query($incr(query))="select names4.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where firstName = 'Zero' order by lastName;"
	set query($incr(query))="select names4.id, firstName, lastName, pastaName from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where favoritePasta = 'Cavatelli' order by firstName;"
	set query($incr(query))="select distinct pastas.id, favoritePasta from names4 inner join pastas on pastas.pastaName = names4.favoritePasta where favoritePasta = 'Cavatelli' order by pastas.id;"
	for i=1:1:query do
	. for type="left","right","full" do
	. . set file="jointest"_$translate($justify(i,2)," ","0")_type_".sql"
	. . open file:(newversion)  use file
	. . write $piece(query(i),"inner",1)_type_$piece(query(i),"inner",2),!	; replace inner join with left join or right join or full join (assumes only one "inner join" usage)
	. . close file
	;
