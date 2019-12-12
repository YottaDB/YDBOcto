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
	set initDone("arithmeticOperator")=0
	set initDone("comparisonOperators")=0

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
	. set query=$$generateQuery()
	. write query,!
	.
	. set file=prefix_i_".sql"
	. open file:(newversion)
	. use file
	. ; Add a line to generated query file given LIMIT exists, and ORDER BY does not in the query
	. ; This forces the crosscheck function to only count lines as having a LIMIT clause without an
	. ; ORDER BY clause can cause different results to be returned by the database
	. if (($find(query," LIMIT ")'=0)&($find(query," ORDER BY ")=0))  write "-- rowcount-only-check",!
	. write query,!
	. close file
	. ; The tableColumn/selectListLVN LVNs exists for each individual query,
	. ; thus they need to be KILLED and NEWED after each query is created
	. kill tableColumn,selectListLVN

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
	new line,nlines,holder,firstData,table,i,i2
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

	; #FUTURE_TODO: Change this code block to not "quit "*"", but rather just "set result="*"" also Issue #385
	; Choose whether to use a wildcard or build a select list
	set randInt=$random(4) ;25% chance to get an asterisk, 75% chance to continue further
	if (randInt=0) do
	. set table=$order(tableColumn(""))
	. set tableColumn(table,"*")=""
	if ((randInt=0)&(curDepth=0)) quit "*"
	; #FUTURE_TODO: Reenable following line and remove preceding line, when Issue #385 is resolved
	;if (randInt=0) quit "*"

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=$random(2)

	; Regular column notation is to be used
	if randInt=0 set toBeAdded=$$chooseColumn

	; Qualifier notation (table.column) is to be used
	if randInt=1  set toBeAdded=$order(tableColumn(""))_"."_$$chooseColumn

	set selectListLVN(toBeAdded)=""
	set result=result_toBeAdded

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$selectList(curDepth)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
innerQuerySelectList(curDepth,alias)
	new randInt,result
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""

	; Qualifier notation (table.column), with the value of the alias variable instead of a table
	set toBeAdded=alias_"."_$$chooseColumn

	set selectListLVN(toBeAdded)=""
	set result=result_toBeAdded

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$innerQuerySelectList(curDepth,alias)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
tableExpression()
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen

	set result=""

	; WHERE
	if $random(2)  set result=result_$$whereClause
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	if $random(2)  set result=result_$$orderbyClause
	if $random(2)  set result=result_$$limitClause

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#from%20clause
fromClause()
	new result,randInt,i,x
	;Constructs a FROM clause from a random number of table references.

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
	new result,randInt
	set result=" WHERE "
	set randInt=$random(8) ; 0,1,2,3,4,5,6,7

	; #FUTURE_TODO: change comparison numbers for the if statements when following if statements in WHERE clause are finished

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	if (randInt=0) do
	. new loopCount,i,leftSide,rightSide,notString,andOrChoice,chosenColumn
	. set loopCount=$random(3)+1 ; value between inclusive 1 and 3
	. for i=1:1:loopCount do
	. . set chosenColumn=$$chooseColumn
	. . set leftSide=""
	. . set rightSide=""
	. . if $random(2)  set leftSide=chosenColumn
	. . else  set leftSide=$$chooseEntry(table,chosenColumn)
	. . ;Only needs to be decided once, as chooseEntry checks to make sure matching type
	. . if $random(2)  set rightSide=chosenColumn
	. . else  set rightSide=$$chooseEntry(table,chosenColumn)
	. . set notString=""
	. . if $random(2) set notString="NOT "
	. .
	. . ; First portion of WHERE, no AND or OR
	. . if (i=1) set result=result_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . ; Following portions of WHERE, with AND or OR separators
	. . if (i'=1) do
	. . . set andOrChoice=$random(2)
	. . . ; AND
	. . . if (andOrChoice=0) do
	. . . . set result=result_" AND "_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . . ; OR
	. . . if (andOrChoice=1) do
	. . . . set result=result_" OR "_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"

	if (randInt=1) do
	. new type,chosenColumn,plusMinus
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set result=result_"(("_chosenColumn_plusMinus_$$chooseEntry(table,chosenColumn)_")="_$$chooseEntry(table,chosenColumn)_")"

	if (randInt=2) do
	. ; Left side of this expression will always be forced to be a varchar,
	. ; the right side of this expression can be either varchar or integer
	. ; This is done as PostgreSQL requires at least one side of a string concatenation
	. ; to be a string/varchar and the other to not be.
	. new leftSide,rightSide,type,entry1,entry2
	.
	. set leftSide=""
	. set rightSide=""
	.
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set leftSide=$$chooseColumn
	. . set type=$$returnColumnType(table,leftSide)
	. set entry1=$$chooseEntry(table,leftSide)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. if $random(2)  set leftSide=$$chooseEntry(table,leftSide)
	.
	. set rightSide=$$chooseColumn
	. set entry2=$$chooseEntry(table,rightSide)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. if $random(2)  set rightSide=$$chooseEntry(table,rightSide)
	.
	. set result=result_"(("_leftSide_"||"_rightSide_")"_$$comparisonOperators_"'"_entry1_entry2_"')"

	if (randInt=3) do
	. ; ... WHERE customer_id=-(-3)
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	. set result=result_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_")"

	if (randInt=4) do
	. ; ... WHERE customer_id=-(-3)
	. ; ... WHERE (math expression) arithmetic operator -(math expression)
	.
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn
	. . set type=$$returnColumnType(table,chosenColumn)
	.
	. set beginning="("_$$chooseEntry(table,chosenColumn)_")"
	.
	. new plusMinus
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set end=plusMinus_"("_plusMinus_$$chooseEntry(table,chosenColumn)_")"
	.
	. set result=result_"("_beginning_" "_$$arithmeticOperator_" "_end_") "_$$comparisonOperators_" "_chosenColumn

	if (randInt=5) do
	. ; ... WHERE EXISTS (SELECT ... query)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set alias="q2"
	. set result=result_notString_"EXISTS ("_$$generateSimpleQuery(alias)_" "_alias_")"

	if (randInt=6) do
	. new notString,entryList,i
	. set chosenColumn=$$chooseColumn
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. for i=1:1:($random($$maxIndex(table)-1)+1) do
	. . set entryList=entryList_$$chooseEntry(table,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. set result=result_chosenColumn_" "_notString_"IN ("_entryList_")"

	; #FUTURE_TODO: Make this pretty/better and improve on the string, maybe focus more on wildcards
	; #FUTURE_TODO: Uncomment when Issue 398 (LIKE with wildcard characters) is fixed
	; #FUTURE_TODO: Maybe have a separate BATs test that only tests this clause as this can create
	;       a lot of varying queries, not sure how to do this though
	;if (randInt=7) do
	;. new chosenColumn,string,i,randInt
	;. ; Forces column to be of type "VARCHAR" as to do a string comparison
	;. set type=""
	;. for  quit:($find(type,"VARCHAR")'=0)  do
	;. . set chosenColumn=$$chooseColumn
	;. . set type=$$returnColumnType(table,chosenColumn)
	;. set string=""
	;. for i=1:1:5 do
	;. . set randInt=$random(6) ; 0,1,2,3,4,5
	;. . if (randInt=0) set string=string_$char($random(27)+64) ; @,A-Z
	;. . if (randInt=1) set string=string_$char($random(26)+97) ; a-z
	;. . if (randInt=2) set string=string_$random(10) ; 0-9
	;. . if (randInt=3) set string=string_"#"
	;. . if (randInt=4) set string=string_"$"
	;. . if (randInt=5) set string=string_"%"
	;. . if (randInt=6) set string=string_"_"
	;. set result=result_chosenColumn_" LIKE '"_string_"'"

	if (randInt=7) do
	. new i
	. set chosenColumn=$$chooseColumn
	. ; ... WHERE column BETWEEN entry1 and entry2
	. set result=result_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#group%20by%20clause
groupbyClause()
	new result,randInt,i
	set result=" GROUP BY "

	set randInt=$random(3)+1
	for i=1:1:randInt do
	. set commaString=" "
	. if (i<randInt) set commaString=", "
	. set result=result_$$chooseColumn()_commaString

	quit result

; #FUTURE_TODO: Expand this with more "aggregrate functions" besides COUNT as there has to
;               be more valid keywords/functions, do this when the HAVING clause is in Octo
; https://ronsavage.github.io/SQL/sql-92.bnf.html#having%20clause
havingClause()
	new result,randInt,max,randFromMax
	set result=" HAVING "

	set max=$$maxIndex(table)
	set randFromMax=$random(max-1)+1

	set result=result_"COUNT(("_$$chooseColumn_") "_$$comparisonOperators_" "_($random(max-1)+1)_")"
	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#order%20by%20clause
orderbyClause()
	new result,randInt,i,holder
	set result=" ORDER BY "

	set holder=" "
	for  quit:(holder="")  do
	. set holder=$order(selectListLVN(holder))
	. set result=result_holder_", "

	; strip off the ", , " at the end of result
	if ($extract(result,$length(result)-3,$length(result))=", , ")  set result=$extract(result,0,$length(result)-4)

	quit result

; Cannot find the link for this
limitClause()
	new result,holder,max,a
	set result=" LIMIT "
	set max=$$maxIndex(table)
	set result=result_($random(max-1)+1)

	quit result

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
	new index,formatted,CC,type,entry,i,randInt,count,maxIndex,randFromMaxIndex

	set maxIndex=$$maxIndex(tableName)

	set randFromMaxIndex=$random(maxIndex-1)+1
	set formatted="^"_tableName_"("_randFromMaxIndex_")"

	; Gets type of selected entry
	set type=$$returnColumnType(tableName,columnName)

	set index=$$returnColumnIndex(tableName,columnName)
	; index will occasionally be set to 0,
	; which causes a blank entry to be returned
	if (index=0)  set index=1

	; randomly selects an entry to be returned
	set entry=$order(data(formatted,index,""))

	; If selected entry is not of "INTEGER" type, pre/append a single quote (')
	; This is done to match PSQL format rules, Octo does not care
	; Depending on how Booleans are to be specified, this might need to be changed
	if ($find(type,"INTEGER")=0) set entry="'"_entry_"'"

	quit entry

returnColumnIndex(tableName,columnName)
	; Returns index of column
	set index=0
	for  quit:($order(columns(tableName,index,""))=columnName)  if $increment(index)
	quit index

returnColumnType(tableName,columnName)
	set index=$$returnColumnIndex(tableName,columnName)
	quit $order(sqlInfo(tableName,index,columnName,""))

maxIndex(tableName)
	set holder=" "
	for maxIndex=1:1  do  quit:(holder="")
	. set a="^"_tableName_"("_maxIndex_")"
	. set holder=$order(data(a,""))
	set maxIndex=maxIndex-1
	quit maxIndex

comparisonOperators()
	if (initDone("comparisonOperators")=0) do
	. set initDone("comparisonOperators")=1
	. set comparisonOperators=-1
	. set comparisonOperators($increment(comparisonOperators))="="
	. set comparisonOperators($increment(comparisonOperators))="!="
	. set comparisonOperators($increment(comparisonOperators))="<"
	. set comparisonOperators($increment(comparisonOperators))=">"
	. set comparisonOperators($increment(comparisonOperators))="<="
	. set comparisonOperators($increment(comparisonOperators))=">="
	. if $increment(comparisonOperators)

	quit comparisonOperators($random(comparisonOperators))

arithmeticOperator()
	if (initDone("arithmeticOperator")=0) do
	. set initDone("arithmeticOperator")=1
	. set arithmeticOperator=-1
	. set arithmeticOperator($increment(arithmeticOperator))="+"
	. set arithmeticOperator($increment(arithmeticOperator))="-"
	. set arithmeticOperator($increment(arithmeticOperator))="*"
	. ;set arithmeticOperator($increment(arithmeticOperator))="/" ; #FUTURE_TODO: Uncomment when Issue 365 (Octo division returns decimal) is resolved
	. set arithmeticOperator($increment(arithmeticOperator))="%"
	. if $increment(arithmeticOperator)

	quit arithmeticOperator($random(arithmeticOperator))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
generateQuery()
	set query="SELECT "
	set query=query_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	set query=query_$$selectList($random(3)+1)
	set query=query_" "_fc
	set query=query_$$tableExpression
	quit query

generateSimpleQuery(alias)
	; This function is missing the call to tableExpression as that has
	; the potential to cause an infinite loop with a bunch of EXISTS
	; statements, probably rare but possible
	merge tableColumnTemp=tableColumn
	merge selectListLVNTemp=selectListLVN
	new tableColumn,selectListLVN

	new query
	set query="SELECT "
	set query=query_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	set query=query_$$innerQuerySelectList($random(3)+1,alias)
	set query=query_" "_fc

	new tableColumn,selectListLVN
	merge tableColumn=tableColumnTemp
	merge selectListLVN=selectListLVNTemp
	quit query
