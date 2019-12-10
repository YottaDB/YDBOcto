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

	set initDone("setQuantifier")=0
	set initDone("chooseTable")=0
	set initDone("chooseColumn")=0
	set initDone("whereClause")=0

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

	new i
	for i=1:1:runCount do
	. set query="SELECT "
	. set query=query_$$setQuantifier
	. set fc=$$fromClause ; fromClause needs to run before selectList
	. set query=query_$$selectList($random(3)+1)
	. set query=query_" "_fc
	. set query=query_$$tableExpression
	. write query,!
	.
	. set file=prefix_i_".sql"
	. open file:(newversion)
	. use file
	. write query,!
	. close file
	. ; The tableColumn LVN exists for each individual query, thus it needs to be killed
	. ; after each query is created
	. kill tableColumn

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
	. ; Filters out comment lines (start with '--' or '#'), and/or blank lines
	. if (($extract(line(i),1)'="#")&($extract(line(i),1)'="")&($extract(line(i),1,2)'="--")) do
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
	. . . . set sqlInfo(tableName,i2,columnName,columnType)=""
	. . . set tableNameEnd=columnNameAndTypeEnd
	. . ; shifted over 1 to remove trailing ")"
	. . set finalColumnAndTypeEnd=$find(line(i),")",columnNameAndTypeStart-1)
	. . ; shifted columnNameAndTypeStart over 1 to remove leading " " shifted finalColumnEnd over 2 to remove trailing ") "
	. . set finalColumnAndType=$extract(line(i),columnNameAndTypeStart+1,finalColumnAndTypeEnd-2)
	. . set finalColumnName=$piece(finalColumnAndType," ")
	. . set finalColumnType=$piece(finalColumnAndType," ",2)
	. . set sqlInfo(tableName,i2,finalColumnName,finalColumnType)=""
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
	. . . . set data(table,i2,firstData)=""
	. . . set holder=" "
	. . . if (i2'=1) do
	. . . . set holder=$piece(line(i),"|",i2)
	. . . . ;If there is a " in the string, remove it
	. . . . if (holder["""") set holder=$extract(holder,1,$find(holder,"""")-2)
	. . . . if (holder'="") set data(table,i2,holder)=""
	quit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Grammar Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#set%20quantifier
setQuantifier(curDepth)
	if (initDone("setQuantifier")=0) do
	. set initDone("setQuantifier")=1
	. set quantifier=-1
	. set quantifier($increment(quantifier))="DISTINCT "
	. set quantifier($increment(quantifier))="ALL "
	. set quantifier($increment(quantifier))=""
	. if $increment(quantifier)
	quit quantifier($random(quantifier))

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
selectList(curDepth)
	new randInt,result
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	; Choose whether to use a wildcard or build a select list
	set randInt=$random(4) ;25% chance to get an asterisk, 75% chance to continue further
	if (randInt=0) do
	. set table=$order(tableColumn(""))
	. set tableColumn(table,"*")=""
	if (randInt=0) quit "*"

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=$random(1)
	set randInt=0 ; forced 0 until Qualifier notation (table.column) is done
	if randInt=0 set result=$$chooseColumn ;Chooses column
	set column=result

	; #FUTURE_TODO: Enable when Qualifier notation (table.column) is to be used
	;if randInt=1 do
	;. ;Call Qualifier (use dot)
	;. ;set result="table"_"."_"column"

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$selectList(curDepth)

	; #FUTURE_TODO: Find a more elegant way to do this, maybe don't insert this comma at all
	; Strip off trailing commas
	if ($extract(result,$length(result)-2)=",")  set result=($extract(result,1,$length(result)-3))

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
tableExpression()
	new randInt
	set randInt=$random(2)

	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen

	; #FUTURE_TODO: Change this structure such that each "clause" can be selected indepently
	;               of each other. IE have a seperate random variable that is either 0 or 1
	;               that controls whether a clause could be added. Similar to how the notString
	;               is implemnted in the WHERE Clause
	set result=""
	; none
	if (randInt=0) set result=""
	; where
	if (randInt=1) set result=$$whereClause
	; #FUTURE_TODO: groupby
	if (randInt=2) set result="this shouldn't be printed"
	; #FUTURE_TODO: having
	if (randInt=3) set result="this shouldn't be printed"
	; #FUTURE_TODO: where groupby
	if (randInt=4) set result="this shouldn't be printed"
	; #FUTURE_TODO: where having
	if (randInt=5) set result="this shouldn't be printed"
	; #FUTURE_TODO: groupby having
	if (randInt=6) set result="this shouldn't be printed"
	; #FUTURE_TODO: where groupby having
	if (randInt=7) set result="this shouldn't be printed"
	; #FUTURE_TODO: orderby clause

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
	;set randInt=$random(2)+1

	; #FUTURE_TODO: Commented out until JOIN functionality is implemented
	;tableColumn = context.allTables
	;for i=1:1:randInt do
	;. set result=result_$$chooseTable
	;. ; Isolate last call to exclude comma without additional string operations
	;. if (randInt>1) do
	;. . set result=result_", "
	;. set randInt=$increment(randInt,-1)
	;result += TableReference(context.allTables, tableColumn)

	set chosenTable=$$chooseTable
	set tableColumn(chosenTable)=""

	quit result_chosenTable

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
whereClause()
	if (initDone("whereClause")=0) do
	. set initDone("whereClause")=1
	. set comparisonOperator=-1
	. set comparisonOperator($increment(comparisonOperator))="="
	. set comparisonOperator($increment(comparisonOperator))="!="
	. set comparisonOperator($increment(comparisonOperator))="<"
	. set comparisonOperator($increment(comparisonOperator))=">"
	. set comparisonOperator($increment(comparisonOperator))="<="
	. set comparisonOperator($increment(comparisonOperator))=">="
	. ;set comparisonOperator($increment(comparisonOperator))="BETWEEN" ; #FUTURE_TODO: Not used currently, but is a valid option
	. ;set comparisonOperator($increment(comparisonOperator))="LIKE" ; #FUTURE_TODO: Not used currently, but is a valid option
	. ;set comparisonOperator($increment(comparisonOperator))="IN" ; #FUTURE_TODO: Not used currently, but is a valid option
	. if $increment(number)

	set notString=""
	if $random(2) set notString="NOT "

	; #FUTURE_TODO: operands can be literals/entries and column on either side
	; #FUTURE_TODO: things like id + 1, column expressions
	; #FUTURE_TODO: unary operations, NOT (id=2)
	; #FUTURE_TODO: Involving boolean operators (expr1 AND expr2, expr1 OR expr2)
	set chosenColumn=$$chooseColumn

	quit " WHERE "_notString_chosenColumn_" "_comparisonOperator($random(comparisonOperator))_" "_$$chooseEntry(table,chosenColumn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Supporting Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

chooseColumn()
	if (initDone("chooseColumn")=0) do
	. new a,currentTable,column,columnCount,x,count
	. for a=0:1:tables-1 do
	. . set currentTable=tables(a)
	. . set initDone("chooseColumn")=1
	. . set column=""
	. . set columnCount=1
	. . set x=""
	. . for  set x=$order(sqlInfo(currentTable,x))  quit:x=""  if $increment(columnCount)
	. . set tableColumnCounts(currentTable)=columnCount
	. . set count=1
	. . set x=""
	. . for  set x=$order(sqlInfo(currentTable,count,x)) do  quit:(count=columnCount)  if $increment(count)
	. . . set columns(currentTable,count,x)=""
	. . . set x=""
	. . kill columns(currentTable,count) ;kills empty last entry in this table
	. set initDone("chooseColumn")=1
	new table,CC,selectedColumn
	set table=$order(tableColumn(""))
	set CC=tableColumnCounts(table)
	set selectedColumn=$order(columns(table,$random(CC-1)+1,$random(CC-1)+1))
	set tableColumn(table,selectedColumn)=""
	quit selectedColumn

chooseEntry(tableName,columnName)
	new index,formatted,CC,temp,type,entry,i,randInt,test

	; Finds index of column
	set index=1
	for  quit:($order(columns(tableName,index,""))=columnName)  if $increment(index)

	set formatted="^"_tableName_"("_index_")"

	; Gets type of selected entry
	set type=$order(sqlInfo(tableName,index,columnName,""))

	; randomly selects an entry to be returned
	set CC=tableColumnCounts(tableName)
	set entry=$order(data(formatted,index,""))

	; If selected entry is not of "INTEGER" type, pre/append a single quote (')
	; This is done to match PSQL format rules, Octo does not care
	; Depending on how Booleans are to be specified, this might need to be changed
	if ($find(type,"INTEGER")=0) set entry="'"_entry_"'"

	quit entry
