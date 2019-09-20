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
; This program generates a random SQL query that uses OUTER JOINs with a join nesting depth up to 4
; using the customers database. And runs this query against postgres and Octo and verifies the outputs
; are identical. Certain features of this test are currently disabled due to pending issues.
; Those lines are marked with a ###TMPDISABLE.
; -----------------------------------------------------------------------------------------------------

genouterjoinqueries	;
	set primarykey("customers")="customer_id"
	set primarykey("orders")="order_id"
	set joinstr(0)="inner join"
	set joinstr(1)="left join"
	set joinstr(2)="right join"
	set joinstr(3)="full join"
	set numqueries=20	; generate 20 queries so as not to take a long time for this test to run in pipeline
	set q=0
	for  do  quit:q=numqueries
	. set numjoins=2+(q#6)	; can be 2-way, 3-way, ... up to 7-way join
	. for i=1:1:numjoins  do
	. . set modulo=$random(2),table(i)=$select(modulo:"customers",1:"orders"),tablealias(i)=$extract(table(i),1)
	. ; choose table names for the join(s) next
	. set sqlquery="select "
	. ; choose select output column list
	. set numcols=0
	. for i=1:1:numjoins  do
	. . set modulo=$random(2)	; currently only two choices are used INNER JOIN and LEFT JOIN
	. . set:(i=numjoins)&('modulo) modulo=1
	. . quit:'modulo
	. . set:numcols sqlquery=sqlquery_","
	. . set sqlquery=sqlquery_tablealias(i)_i_"."_primarykey(table(i))  if $incr(numcols)
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
	. set sqlquery=sqlquery_";"
	. ; The below if check is because postgres issues the following error in this case
	. ;	--> ERROR:  FULL JOIN is only supported with merge-joinable or hash-joinable join conditions
	. quit:fulljoinchosen&notequalchosen
	. quit:notequalchosen			; ###TMPDISABLE until #311 is fixed
	. set file="jointest"_$translate($justify($incr(q),2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write sqlquery,!
	. close file
	quit
	;
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
