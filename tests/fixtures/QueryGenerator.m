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

	; Glossary:
	; WHITELIST - A list of acceptable tables to select columns/row information/other data
	;             from. Acceptable tables being those that were selected for use in the
	;             query, from which columns and data can be pulled from, ensuring valid queries.

	set initDone("setQuantifier")=0
	set initDone("chooseTable")=0
	set initDone("chooseColumn")=0

	set arguments=$zcmdline
	set sqlFile=$piece(arguments," ",1)
	if (sqlFile="") set sqlFile="customers.sql"
	set zwrFile=$piece(arguments," ",2)
	if (zwrFile="") set zwrFile="customers.zwr"
	set runCount=$piece(arguments," ",3)
	if (runCount="") set runCount=1
	set prefix=$piece(arguments," ",4)
	if (prefix="") set prefix="query"

	do readSQL(sqlFile)
	do readZWR(zwrFile)

	for i=1:1:runCount do
	. set query="SELECT "
	. set query=query_$$setQuantifier
	. set fc=$$fromClause
	. set query=query_$$selectList
	. set query=query_" "_fc
	. write query,!
	.
	. set file=prefix_i_".sql"
	. open file:(newversion)
	. use file
	. write query,!
	. close file

	quit

;Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
readSQL(file)
	new line,nlines,count,tableNameStart,tableNameEnd,tableName,columnNameAndTypeStart,columnNameAndTypeEnd,columnName
	new columnType,columnNameAndTypeStart,finalColumnAndTypeEnd,finalColumnName,finalColumnType,columnNameAndType,i,nlines,i2
	open file:(readonly)
	use file
	for  read line($increment(nlines))  quit:$zeof
	close file
	kill line(nlines)  if $increment(nlines,-1) ;kills line that contains EOF

	for i=1:1:nlines do
	. if ($extract(line(i),1)'="#")&($extract(line(i),1)'="") do
	. . ;TABLE NAMES
	. . set tableNameStart=$find(line(i),"CREATE TABLE ")
	. . set tableNameEnd=$find(line(i)," ",tableNameStart)
	. . set tableName=$extract(line(i),tableNameStart,tableNameEnd-2)
	. .
	. . ;COLUMN NAMES
	. . for i2=1:1 do  quit:(columnNameAndTypeEnd=0)
	. . . set columnNameAndTypeStart=$find(line(i),"",tableNameEnd)
	. . . set columnNameAndTypeEnd=$find(line(i),",",tableNameEnd)
	. . . if ($extract(line(i),columnNameAndTypeStart,columnNameAndTypeEnd)'="") do
	. . . . ; shifted columnNameStart over 1 to remove leading " " and "(" shifted columnNameEnd over 2 to remove trailing ", "
	. . . . set columnNameAndType=$extract(line(i),columnNameAndTypeStart+1,columnNameAndTypeEnd-2)
	. . . . set columnName=$piece(columnNameAndType," ")
	. . . . set columnType=$piece(columnNameAndType," ",2)
	. . . . set sqlInfo(tableName,columnName,columnType)="test"
	. . . set tableNameEnd=columnNameAndTypeEnd
	. . ; shifted over 1 to remove trailing ")"
	. . set finalColumnAndTypeEnd=$find(line(i),")",columnNameAndTypeStart-1)
	. . ; shifted columnNameAndTypeStart over 1 to remove leading " " shifted finalColumnEnd over 2 to remove trailing ") "
	. . set finalColumnAndType=$extract(line(i),columnNameAndTypeStart+1,finalColumnAndTypeEnd-2)
	. . set finalColumnName=$piece(finalColumnAndType," ")
	. . set finalColumnType=$piece(finalColumnAndType," ",2)
	. . set sqlInfo(tableName,finalColumnName,finalColumnType)=""
	quit

readZWR(file)
	new line,nlines,holder,firstData,table
	open file:(readonly)
	use file
		for  read line($increment(nlines))  quit:$zeof
	close file
	kill line(nlines)  if $increment(nlines,-1)

	for i=1:1:nlines do
	. if ($extract(line(i),1)="^") do
	. . for i2=1:1 do  quit:(holder="")
	. . . set firstData=""
	. . . if (i2=1) do
	. . . . set table=$piece(line(i),"=""")
	. . . . set firstData=$piece($piece(line(i),"=""",2),"|")
	. . . . set data(table,firstData)=""
	. . . set holder=" "
	. . . if (i2'=1) do
	. . . . set holder=$piece(line(i),"|",i2)
	. . . . ;If there is a " in the string, remove it
	. . . . if (holder["""") set holder=$extract(holder,1,$find(holder,"""")-2)
	. . . . if (holder'="") set data(table,holder)=""
	quit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Grammar Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#set%20quantifier
setQuantifier()
	if (initDone("setQuantifier")=0) do
	. set initDone("setQuantifier")=1
	. set quantifier=-1
	. set quantifier($increment(quantifier))="DISTINCT "
	. set quantifier($increment(quantifier))="ALL "
	. set quantifier($increment(quantifier))=""
	. if $increment(quantifier)
	quit quantifier($random(quantifier))

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
selectList()
	new randInt,result
	;This function serves the same purpose as select sublist in the grammar rules for SQL.
	;#FUTURE_TODO: Recursively generates a random select list.
	;:returns result: A string representing a complete select list.

	; Choose whether to use a wildcard or build a select list
	set randInt=$random(4) ;25% chance to get an asterisk, 75% chance to continue further
	if (randInt=0) quit "*"

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=$random(1)
	set randInt=0 ; forced 0 until Qualifier/table.column notation is done
	if randInt=0 set result=$$chooseColumn ;Chooses column

	; #FUTURE_TODO: change what this "returns" when ready
	if randInt=1 do
	. ;Call Qualifier (use dot)
	. ;set result="table"_"."_"column"
	. set result=$$chooseColumn

	; #FUTURE_TODO:
	;if cur_depth > 0:
	;. cur_depth=cur_depth-1 ; to drop down a "level" in depth
	;. set result=result_", "
	;. $$selectList

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#from%20clause
fromClause()
	new result,randInt,i,x
	;Constructs a FROM clause from a random number of table references.
	;:returns result: A string representing a derived column, i.e. column name.

	set result="FROM "
	; Choose number of table references to include based on the the maximum
	; recursion depth. This limit is arbitrary, but should be reasonable
	; provided a recursion depth < 10.
	;randInt = random.randint(1, context.max_depth)
	set randInt=$random(1)

	; #FUTURE_TODO: Commented out until WHITELIST functionality is implemented
	;whitelist = context.allTables
	;for i in range(0, randInt-1):
	;. result += TableReference(context.allTables, whitelist)
	;. ; Isolate last call to exclude comma without additional string operations
	;. if seed > 1:
	;. . result += ", "
	;. seed -= 1
	;result += TableReference(context.allTables, whitelist)

	set result=result_$$chooseTable
	quit result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Supporting Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; #FUTURE_TODO: This contains no logic, when WHITELIST is added more logic can be added
chooseTable()
	if (initDone("chooseTable")=0) do
	. set initDone("chooseTable")=1
	. new x,count
	. set table=""
	. set tables=0
	. set x=""
	. for  set x=$order(sqlInfo(x))  set:(x'="") tables=tables+1  quit:x=""
	. set count=-1
	. set x=""
	. for  set x=$order(sqlInfo(x))  quit:x=""  set tables($increment(count))=x
	set table=tables($random(tables))
	quit table

; #FUTURE_TODO: This contains no (very minimal) logic, when WHITELIST is added more logic can be added
chooseColumn()
	if (initDone("chooseColumn")=0) do
	. new a
	. for a=0:1:tables-1 do
	. . set currentTable=tables(a)
	. . set initDone("chooseColumn")=1
	. . new column,x,count
	. . set column=""
	. . set columnCount=0
	. . set x=""
	. . for  set x=$order(sqlInfo(currentTable,x))  quit:x=""  if $increment(columnCount)
	. . set count=0
	. . set x=""
	. . for  set x=$order(sqlInfo(currentTable,x)) do  quit:x=""  if $increment(count)
	. . . set columns(currentTable,count)=x
	. set initDone("chooseColumn")=1
	quit columns(table,$random(columnCount))
