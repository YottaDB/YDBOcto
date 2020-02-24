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
	; The variable "currentAlias" maintains the parent queries alias number, it does not track
	; any of the subquery alias number changes that do not apply to the parent query. It does not
	; wrap any function calls pertaining to only the subqueries, namely any of the inner/subquery
	; specific functions and a few of the cases of the "havingClause" function, which is done differently
	; as a result of coding the "havingClause" function to handle both HAVINGs occuring within the parent
	; query and the subquery.

	; #FUTURE_TODO: Implement subqueries into other places (Group By, Having, etc.). Example below
	;               SELECT n1.id from names n1 where n1.id * (select n2.id from names n2 where n2.id = n1.id % 3) = n1.id * 2;
	; #FUTURE_TODO: Make it so subqueries from the select list can be used in other places (Assuming that they can)
	;               EX: Select (subquery) AS alias FROM table WHERE alias = 1;
	; #FUTURE_TODO: Add aliases into FROM clause like example query below.
	;               SELECT n1.id FROM names n1;
	; #FUTURE_TODO: Add typecast operator (::) to valid places, example usage below
	;               https://www.postgresql.org/docs/9.4/sql-expressions.html#SQL-SYNTAX-TYPE-CASTS
	;               New: CAST ("123" as INTEGER)  Old (SQL-92 Compliant): "123"::INTEGER
	; #FUTURE_TODO: Add TRUE and FALSE into any place that has expressions (WHERE, ON, HAVING, ...)
	;               Started with version 9 of WHERE clause
	;               WHERE ((id = 1) = TRUE)
	; #FUTURE_TODO: Boolean is now a supported type, test with boolean.sql and boolean.zwr
	;               EX: WHERE (NOT) booleanColumn
	;               Currently disabled until issue 346 is resolved
	; #FUTURE_TODO: Expand aggregate functions
	;               SUM(column) -> SUM(column1 + column2 ... + 1 * 100)
	; #FUTURE_TODO: Put aggregate functions into other places in the query
	; #FUTURE_TODO: Combine whereClause(), innerWhereClause(), and havingClause as they are all nearly identical to each other
	; #FUTURE_TODO: Add the NULL value to various comparisons
	;               Example: column IS NULL or WHERE column = NULL
	; #FUTURE_TODO: Test to see if CASEs comparisons can be made against other CASEs, if so
	;               implement this into WHERE and HAVING clauses

	set initDone("setQuantifier")=0
	set initDone("chooseTable")=0
	set initDone("chooseColumn")=0
	set initDone("arithmeticOperator")=0
	set initDone("comparisonOperators")=0
	set initDone("booleanOperator")=0
	set initDone("joinTypes")=0
	set initDone("tf")=0
	set initDone("aas")=0
	set initDone("aggregrateFunctions")=0
	set GLOBALtotalTables=0
	set orderByExists="FALSE"
	set limitExists="FALSE"
	set outerJoinExists="FALSE"
	set outerJoinsAllowed="FALSE"
	set existsInHavingExists="FALSE"
	set caseFunctionExists="FALSE"

	set arguments=$zcmdline
	set sqlFile=$piece(arguments," ",1)
	if (sqlFile="") set sqlFile="customers.sql"
	set zwrFile=$piece(arguments," ",2)
	if (zwrFile="") set zwrFile="customers.zwr"
	; The runCount variable determines the amount of .sql files to be generated when this file is ran.
	set runCount=$piece(arguments," ",3)
	if (runCount="") set runCount=1
	; The prefix variable defines the prefix for the .sql files in the form of "prefix###.sql"
	set prefix=$piece(arguments," ",4)
	if (prefix="") set prefix="query"

	do readSQL($$findFile(sqlFile))
	do readZWR($$findFile(zwrFile))

	do checkForEmptyTables()

	;Main Loop
	new i
	for i=1:1:runCount do
	. set aliasNum=0
	. set currentAlias=0
	. set fromNum=1
	.
	. ; About 1/5 of queries generated will contain GROUP BY and will also
	. ; match the requirements for having a GROUP BY in the query
	. ; #FUTURE_TODO: Uncomment following 2 lines when issue 311 is resolved, aggregate functions
	. ;               cause issues when there are null strings in the database being queried
	. ;set allowGBH("alias"_aliasNum)="FALSE"
	. ;if '($random(5))  set allowGBH("alias"_aliasNum)="TRUE"
	.
	. set query=""
	. set query=$$generateQuery
	. write query,!
	.
	. set file=prefix_i_".sql"
	. open file:(newversion)
	. use file
	. ; Add a line to generated query file given LIMIT exists, and ORDER BY does not in the query
	. ; This forces the crosscheck function to only count lines as having a LIMIT clause without an
	. ; ORDER BY clause can cause different results to be returned by the database
	. if ((limitExists="TRUE")&(orderByExists="FALSE"))  write "-- rowcount-only-check",!
	. write query,!
	. close file
	. ; The following LVNs exist for each individual query,
	. ; thus they need to be KILLED after each query is created
	. kill tableColumn,selectListLVN,subQuerySelectedTables,tableColumnCopy,innerQuerySLLVN,dontJoin
	. ; The following variables need to be reset to their default values for each, seperate query.
	. set orderByExists="FALSE"  set limitExists="FALSE"  set outerJoinExists="FALSE" set outerJoinsAllowed="FALSE"
	. set existsInHavingExists="FALSE"  set caseFunctionExists="FALSE"
	. set currentAlias=0

	quit

; #FUTURE_TODO: Look into using $piece functions instead of $extract and $find for parsers,
;               as it might help to improve the readability of the semi-messy parsers
;Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This function searches for, and returns, the relative path of the fixture .sql and .zwr files inside of the Octo folder structure.
; It takes in the filename of the desired .sql/.zwr file as a parameter.
findFile(file)
	if ($zsearch(file)="") do
	. set file="../../tests/fixtures/"_file
	quit file

; This function takes in a .sql file, and stores table names, row names, and the variable typing of the rows.
; It takes in a file, or file path, to the .sql file as a parameter.
readSQL(file)
	new line,nlines,count,tableNameStart,tableNameEnd,tableName,columnNameAndTypeStart,columnNameAndTypeEnd,columnName
	new columnType,columnNameAndTypeStart,finalColumnAndTypeEnd,finalColumnName,finalColumnType,columnNameAndType,i,nlines,i2
	open file:(readonly)
	use file
	for  read line($increment(nlines))  quit:$zeof
	close file
	kill line(nlines)  if $increment(nlines,-1) ;kills line that contains EOF

	set i2=1
	for i=1:1:nlines do
	. ; Filters out comment lines (start with '--' or '#'), and/or blank lines
	. ; Also filters out lines that contain the word "EXTRACT"
	. if (($extract(line(i),1)'="#")&($extract(line(i),1)'="")&($extract(line(i),1,2)'="--"))&($find(line(i)," EXTRACT ")=0) do
	. . ;TABLE NAMES
	. . if ($find(line(i),"CREATE TABLE")'=0) do
	. . . set tableNameStart=$find(line(i),"CREATE TABLE ")
	. . . set tableNameEnd=$find(line(i)," ",tableNameStart)
	. . . set tableName=$extract(line(i),tableNameStart,tableNameEnd-2)
	. . . if ($extract(tableName,0,1)="`")  set tableName=$extract(tableName,2,$length(tableName)-1)
	. . . if $increment(GLOBALtotalTables)
	. .
	. . ;set i2=1
	. . ;COLUMN NAMES
	. . if (($find(line(i),"INTEGER")'=0)!($find(line(i),"VARCHAR")'=0)!($find(line(i),"BOOLEAN")'=0)) do
	. . . for  do  quit:(columnNameAndTypeEnd=0)
	. . . . set columnNameAndTypeStart=$find(line(i),"",tableNameEnd)
	. . . . set columnNameAndTypeEnd=$find(line(i),",",tableNameEnd)
	. . . . if ($extract(line(i),columnNameAndTypeStart,columnNameAndTypeEnd)'="") do
	. . . . . ; shifted columnNameStart over 1 to remove leading " " and "(" shifted columnNameEnd over 2 to remove trailing ", "
	. . . . . set columnNameAndType=$extract(line(i),columnNameAndTypeStart+1,columnNameAndTypeEnd-2)
	. . . . . set columnName=$piece(columnNameAndType," ")
	. . . . . set columnType=$piece(columnNameAndType," ",2)
	. . . . . set sqlInfo(tableName,i2,columnName,columnType)=""
	. . . . set tableNameEnd=columnNameAndTypeEnd
	. . . . if $increment(i2)
	. . . if $increment(i2,-1) ; i2 needs to be 1 less at this point, as the above loop runs until a blank line is found
	. . . if ($find(line(i),";")'=0)  do
	. . . . ; shifted over 1 to remove trailing ")"
	. . . . set finalColumnAndTypeEnd=$find(line(i),")",columnNameAndTypeStart-1)
	. . . . ; shifted columnNameAndTypeStart over 1 to remove leading " " shifted finalColumnEnd over 2 to remove trailing ") "
	. . . . set finalColumnAndType=$extract(line(i),columnNameAndTypeStart+1,finalColumnAndTypeEnd-2)
	. . . . set finalColumnName=$piece(finalColumnAndType," ")
	. . . . set finalColumnType=$piece(finalColumnAndType," ",2)
	. . . . set sqlInfo(tableName,i2,finalColumnName,finalColumnType)=""
	. . . . ; if there is no semicolon in line(i) reset i2 to be used again, else keep i2 the same
	. . . . if ($find(line(i),";",finalColumnAndTypeEnd)'=0)  set i2=1
	. set tableNameEnd=0
	quit

; This function takes in the a .zwr file, and stores the data contained within the tables/rows relative to their table name and row name.
; It takes in a file, or file path, to the .zwr file as a parameter.
readZWR(file)
	new line,nlines,holder,firstData,table,i,i2,previous
	open file:(readonly)
	use file
		for  read line($increment(nlines))  quit:$zeof
	close file
	kill line(nlines)  if $increment(nlines,-1)

	set innerNumber=1
	set previous=""
	for i=1:1:nlines do
	. if ($extract(line(i),1)="^") do
	. . for i2=1:1 do  quit:(holder="")
	. . . set firstData=""
	. . . if (i2=1) do
	. . . . set fullThing=$piece(line(i),"=""") ; EX: ^Names(###)
	. . . . if ($extract($piece(fullThing,"("),2,1000000)'=previous)  set innerNumber=1
	. . . . set previous=$extract($piece(fullThing,"("),2,1000000)
	. . . . set table=$piece(fullThing,"(")_"("_innerNumber_")" ; EX: ^Names(custom number)
	. . . . if $increment(innerNumber)
	. . . . set firstData=$piece($piece(line(i),"=""",2),"|")
	. . . . set data(table,i2,$piece($piece(fullThing,"(",2),")"))=""
	. . . . set data(table,i2+1,firstData)=""
	. . . set holder=" "
	. . . if (i2'=1) do
	. . . . set holder=$piece(line(i),"|",i2)
	. . . . ;If there is a " in the string, remove it
	. . . . if (holder["""") set holder=$extract(holder,1,$find(holder,"""")-2)
	. . . . if (holder'="") set data(table,i2+1,holder)=""
	. . . set pKey=$extract(line(i),$find(line(i),"("),$find(line(i),")")-2)
	. . . set primaryKeys($extract(table,0,$find(table,"(")-2),pKey)=""
	quit

checkForEmptyTables()
	; If a table "exists" but has no data (such as namesWithAges in names db),
	; then this function will remove it
	new holder,one,two

	set holder=" "
	for  do  quit:(holder="")
	. set holder=$order(sqlInfo(holder))
	. if (holder'="")  set sqlPart(holder)=""

	set holder=" "
	for  do  quit:(holder="")
	. set holder=$order(data(holder))
	. if (holder'="")  set zwrPart($extract(holder,2,$find(holder,"(")-2))=""

	set one=" "
	set two=" "
	for  do  quit:(one="")
	. set one=$order(zwrPart(one))
	. for  do  quit:(two="")
	. . set two=$order(sqlPart(two))
	. . if ($data(sqlPart(one))'=$data(zwrPart(two))&(two'="")&(one'=""))  kill sqlPart(two),sqlInfo(two)  if $increment(GLOBALtotalTables,-1)

	quit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Grammar Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#set%20quantifier
; This function returns a randomly selected quantifer for either the parent query, or any subsequent subquery.
setQuantifier(curDepth)
	if (initDone("setQuantifier")=0) do
	. set initDone("setQuantifier")=1
	. set quantifier=-1
	. set quantifier($increment(quantifier))="DISTINCT "
	. set quantifier($increment(quantifier))="ALL "
	. set quantifier($increment(quantifier))=""
	. if $increment(quantifier)
	; #FUTURE_TODO: The following if/else block can likely be removed when issue 311 is
	;               resolved, but this will need to be tested as the if/else block prevents
	;               the "DISTINCT" quantifer from being used in a query containing aggregate
	;               functions
	; #FUTURE_TODO: Uncomment following if/else/quit block when aggregate functions are to be
	;               re-enabled (related to issue 311), aggregate functions cause issues
	;               when there are null strings in the database being queried
	;if (allowGBH("alias"_aliasNum)="TRUE") do
	;. for  do  quit:(actualQuantifier'="DISTINCT ")
	;. . set actualQuantifier=quantifier($random(quantifier))
	;else  set actualQuantifier=quantifier($random(quantifier))
	;quit actualQuantifier
	; #FUTURE_TODO: Remove below 3 lines when #311 is fixed
	set actualQuantifier=quantifier($random(quantifier))
	set quantiferLVN("alias"_aliasNum)=actualQuantifier
	quit actualQuantifier

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
; This function returns a select list only for the parent query. Currently it can only contain table.column, subquery, and case functions.
; It takes in a curDepth value, since it is recursive. This value is continously lowered until it reaches 0, thus ending the recursive loop.
; It stores every value added into the SELECT LIST into a LVN called selectListLVN.
selectList(curDepth)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=""

	; #FUTURE_TODO: Uncomment line containing the "if" when issue 311 is resolved, and remove
	;               line missing the "if" statement that follows the one with the "if" statement
	;if (allowGBH("alias"_"0")'="TRUE")  set randInt=$random(9) ; 0-8 ; #FUTURE_TODO: Increase to 10 when issues #385 and #386 are resolved
	set randInt=$random(9) ; 0-8 ; #FUTURE_TODO: Increase to 10 when issues #385 and #386 are resolved

	; To avoid ambiquity warnings, this is commented out
	; Regular column notation is to be used
	;if randInt=0 set toBeAdded=$$chooseColumn("")

	; Below check is to ensure that if there is only 1 table, that there can be no subquerying
	if (GLOBALtotalTables=1) set randInt=0

	; Qualifier notation (table.column) is to be used
	if ((randInt=0)!(randInt=1)!(randInt=2))  do
	. set table=$order(tableColumn(1))
	. set toBeAdded=table_"."_$$chooseColumn(table)
	. set selectListLVN(toBeAdded)=""

	if ((randInt=3)!(randInt=4)!(randInt=5))  do
	. set aliasNum1More=aliasNum+1
	. set alias="alias"_aliasNum1More
	. if $increment(currentAlias)
	. set toBeAdded="("_$$generateSubQuery("limited")_") AS "_alias
	. if $increment(currentAlias,-1)
	. set selectListLVN(alias)=""

	if ((randInt=6)!(randInt=7)!(randInt=8))  do
	. set toCompare=$random(4)+1
	. set toBeAdded=$$returnCaseFunction("SELECT LIST","randomNumbers","numbers","FALSE",toCompare)
	. set selectListLVN("alias"_aliasNum)=""

	if (randInt=9)  do
	. set toBeAdded="*"
	. set selectListLVN(toBeAdded)=""

	; #FUTURE_TODO: Uncomment following code block when issue 311 is resolved, aggregate functions
	;               cause issues when there are null strings in the database being queried
	;if (allowGBH("alias"_"0")="TRUE")  do
	;. new table,chosenColumn,agg,chosenColumn2,tc
	;. ; #FUTURE_TODO: Remove following line when issues (both in Octo and in test_helpers)
	;. ;               pertaining to aggregate functions are resolved
	;. set curDepth=0
	;.
	;. set table=$order(tableColumn(1))
	;. set chosenColumn=$$chooseColumn(table)
	;. set agg=$$returnAggregateFunction(table,chosenColumn,"SL")
	;.
	;. for  do  quit:(chosenColumn'=chosenColumn2)
	;. . set chosenColumn2=$$chooseColumn(table)
	;. set tc=table_"."_chosenColumn2
	;.
	;. set toBeAdded=agg_", "_tc
	;. set selectListLVN(tc)=""

	set result=result_toBeAdded

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$selectList(curDepth)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
; This function controls whether or not to add various other portions of a query into the parent query. This includes WHERE, GROUP BY, HAVING, ORDER BY, and LIMIT.
tableExpression()
	new result
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen
	set result=""

	; #FUTURE_TODO: Avoid WHERE clause selection if OUTER JOINs are used in the query (related to Issue 311).
	;               When 311 is resolved, the line containing `$find` calls below (to search for OUTER JOIN usages)
	;               can be removed and the line after it can instead be uncommented.
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$whereClause
	;if (($random(2))  set result=result_$$whereClause
	; #FUTURE_TODO: Uncomment following line when issue 311 is resolved, aggregate functions
	;               cause issues when there are null strings in the database being queried
	;if (allowGBH("alias"_"0")="TRUE")  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when issue 311 is resolved, aggregate functions
	;               cause issues when there are null strings in the database being queried
	;if (allowGBH("alias"_"0")="TRUE")  set result=result_$$havingClause("query")
	; #FUTURE_TODO: Avoid ORDER BY clause selection if OUTER JOINs are used in the query (related to Issue 311).
	;               When 311 is resolved, the line containing `$find` calls below (to search for OUTER JOIN usages)
	;               can be removed and the line after it can instead be uncommented.
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$orderbyClause(0,"QUERY")  set orderByExists="TRUE"
	; #FUTURE_TODO: Remove preceding line, and uncomment the following commented out line
	;               when Issue 311 is resolved
	;if ($random(2))  set result=result_$$orderbyClause(0,"QUERY")  set orderByExists="TRUE"
	if '($random(8))  set result=result_$$limitClause  set limitExists="TRUE"

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#from%20clause
; #FUTURE_TODO: Implement subqueries into FROM clause
; This function returns the fromClause for the parent query. Currently it only supports a single table to be pulled from, but contains code that should enable multiple tables to be pulled from at a time.
fromClause()
	new result,randInt,i,x,chosenTable

	set result="FROM "

	; This is much harder than first thought, a variety of different issues
	; #FUTURE_TODO: This works from a functionality standpoint, the issue is that when a JOIN clause
	;               is also in the query, the tables in the fromClause overlap with those in the JOIN
	;               The tables in the fromClause are the same as those that are to be joined, which
	;               Octo and PostgreSQL both do not like. This can be resolved using aliases.
	;               This TODO requires a lot more work than previously thought, a large number of
	;               other functions will need to be rewritten in order to handle having a list of FROMS.
	;               Going into the future, these changes will likely need to be done in a separate merge
	;               request, as it appears to need a lot of changes in order to get functioning properly.
	;               Example: SELECT * FROM customers, orders RIGHT JOIN orders on (order_id=customer_id)
	;set randInt=$random(GLOBALtotalTables)+1
	;set fromTableList=""
	;set toBeAdded=""
	;for i=1:1:randInt do
	;. for  quit:($find(fromTableList,toBeAdded)=0)  set toBeAdded=$$chooseTable
	;. ;set toBeAdded=$$chooseTable
	;. ;if toBeAdded not in tableColumn
	;. ;if ($find(fromTableList,toBeAdded)=0) do
	;. set fromTableList=fromTableList_toBeAdded
	;. if (i>1) do
	;. . set fromTableList=fromTableList_" AS alias"_aliasNum
	;. ; Isolate last call to exclude comma without additional string operations
	;. if (randInt>1) do
	;. . set fromTableList=fromTableList_", "
	;. set tableColumn(toBeAdded)=""
	;. set randInt=$increment(randInt,-1)

	set chosenTable=""
	for  do  quit:($data(tableColumnCopy(chosenTable))=0)
	. set chosenTable=$$chooseTable

	; Following two commented out lines, and the comments at the end of the quit statement
	; are intended to support the eventual change to have a list of tables to be selected from.
	;if $increment(fromNum)
	;set tableColumn("f"_fromNum)=chosenTable
	set tableColumn(chosenTable)=""

	quit result_chosenTable ;_"f"_fromNum ;chosenTable OR fromTableList

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
; This function returns the WHERE clause, which contains the WHERE statement as well as a randomly selected condition type.
whereClause()
	new result,randInt,i,x
	set result=" WHERE "
	set randInt=$random(12) ; 0-11 ; Increase this range as new versions of WHERE clauses are added

	set table=$piece(fc," ",2)

	set type=""

	; When randInt=2 or 8 a VARCHAR type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 2 is string concatenation, which requires at least one VARCHAR in order to function
	; WHERE clause type 8 is LIKE with wildcards, this comparison can only occur on VARCHARs, not numeric/integer/boolean
	if ((randInt=2)!(randInt=8))  do
	. set x="sqlInfo("""_table_""")"
	. for i=1:1  do  quit:($find(type,"VARCHAR")'=0)
	. . set x=$query(@x)
	. . set type=$qsubscript(x,4)
	. . if ($qsubscript(x,1)'=table)  set randInt=0

	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	; When randInt=999 a BOOLEAN type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 999 is just "WHERE boolean-type-column"
	; Currently commented out until issue 346 is resolved
	;if (randInt=999)  do
	;. set x="sqlInfo("""_table_""")"
	;. for i=1:1:15  do  quit:(($find(type,"BOOLEAN")'=0)!(x=""))
	;. . set x=$query(@x)
	;. . if (x'="")  set type=$qsubscript(x,4)
	;. . if ((x'="")&($qsubscript(x,1)'=table))  set randInt=0
	;. if ((i=15)!(x=""))  set randInt=0

	if (randInt=0) do
	. new loopCount,i,leftSide,rightSide,notString,chosenColumn,opened
	. set loopCount=$random(3)+1 ; 1-3
	. set opened="FALSE"
	. for i=1:1:loopCount do
	. . set chosenColumn=$$chooseColumn(table)
	. . set leftSide=""
	. . set rightSide=""
	. . if $random(2)  set leftSide=table_"."_chosenColumn
	. . else  set leftSide=$$chooseEntry(table,chosenColumn)
	. . if $random(2)  set rightSide=table_"."_chosenColumn
	. . else  set rightSide=$$chooseEntry(table,chosenColumn)
	. . set notString=""
	. . if $random(2) set notString="NOT "
	. .
	. . ; First portion of WHERE, no AND or OR
	. . if (i=1) set result=result_"("_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . ; Following portions of WHERE, with AND or OR separators
	. . if (i'=1) do
	. . . set result=result_" "_$$booleanOperator_" "
	. . . if (($random(2))&(opened="FALSE"))  do
	. . . . set result=result_"("
	. . . . set opened="TRUE"
	. . . set result=result_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . . if (($random(2))&(opened="TRUE"))  do
	. . . . set result=result_")"
	. . . . set opened="FALSE"
	. . if (i=loopCount)  set result=result_")"
	. . if ((i=loopCount)&(opened="TRUE"))  set result=result_")"

	if (randInt=1) do
	. new condition,type,chosenColumn,plusMinus
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set result=result_"(("_table_"."_chosenColumn_plusMinus_$$chooseEntry(table,chosenColumn)_")="_$$chooseEntry(table,chosenColumn)_")"

	if (randInt=2) do
	. new leftSide,rightSide,type,entry1,entry2
	. ; Left side of this expression will always be forced to be a varchar,
	. ; the right side of this expression can be either varchar or integer
	. ; This is done as PostgreSQL requires at least one side of a string concatenation
	. ; to be a string/varchar and the other to not be.
	. new leftSide,rightSide,type,chosenColumn1,chosenColumn2,entry1,entry2
	.
	. set leftSide=""
	. set rightSide=""
	.
	. set chosenColumn1=""
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn1=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn1)
	. set entry1=$$chooseEntry(table,chosenColumn1)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. set leftSide=table_"."_chosenColumn1
	. if $random(2)  set leftSide=$$chooseEntry(table,chosenColumn1)
	.
	. set chosenColumn2=$$chooseColumn(table)
	. set entry2=$$chooseEntry(table,chosenColumn2)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. set rightSide=table_"."_chosenColumn2
	. if $random(2)  set rightSide=$$chooseEntry(table,chosenColumn2)
	.
	. set result=result_"(("_leftSide_"||"_rightSide_")"
	. set result=result_$$comparisonOperators
	. set result=result_"'"_entry1_entry2_"')"

	if (randInt=3) do
	. new type,chosenColumn,plusMinus,plusMinus2
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	. set result=result_"("_table_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_"))"

	if (randInt=4) do
	. new type,chosenColumn,beginning,plusMinus,end,aOperator
	.
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn)
	.
	. set beginning="("_$$chooseEntry(table,chosenColumn)_")"
	.
	. new plusMinus
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	.
	. set endEntry=$$chooseEntry(table,chosenColumn)
	.
	. ; Checks if the selected operator is division/modulo and then checks if the
	. ; divisor is 0, if it is 0, it then forces it to a non zero number (1)
	. set aOperator=$$arithmeticOperator
	. if (((aOperator="/")!(aOperator="%"))&(endEntry=0))  set endEntry=1
	.
	. set end=plusMinus_"("_plusMinus_endEntry_")"
	.
	. set result=result_"(("_beginning_" "_aOperator_" "_end_") "_$$comparisonOperators_" "_table_"."_chosenColumn_")"

	if (randInt=5) do
	. new notString,alias
	. set notString=""
	. if $random(2) set notString="NOT "
	. if $increment(currentAlias)
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("full")_")"
	. if $increment(currentAlias,-1)

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$chooseColumn(table)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. set limit=($random($$maxIndex(table)-1)+1)
	. if (limit>8)  set limit=8
	. for i=1:1:limit do
	. . set entryList=entryList_$$chooseEntry(table,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. set result=result_table_"."_chosenColumn_" "_notString_"IN ("_entryList_")"

	if (randInt=7) do
	. new chosenColumn
	. set chosenColumn=$$chooseColumn(table)
	. set result=result_table_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	if (randInt=8) do
	. new chosenColumn,string,i,randInt
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn)
	. set string=""
	. set desiredStringLength=$random(7)+1 ; 1-7
	. for i=1:1:desiredStringLength do
	. . set randInt=$random(9) ; 0-8
	. . if ((randInt=0)!(randInt=1)) set string=string_$char($random(27)+64) ; @,A-Z
	. . if ((randInt=2)!(randInt=3)) set string=string_$char($random(26)+97) ; a-z
	. . if (randInt=4) set string=string_$random(10) ; 0-9
	. . if (randInt=5) set string=string_"#"
	. . if (randInt=6) set string=string_"$"
	. . if (randInt=7) set string=string_"%"
	. . if (randInt=8) set string=string_"_"
	. set result=result_table_"."_chosenColumn_" LIKE '"_string_"'"

	if (randInt=9) do
	. new randInt,word,leftSide,rightSide,chosenColumn,entryList,limit,rightType
	. set word=$$aas
	. set leftSide=$$chooseColumn(table)
	. set leftType=$$returnColumnType(table,leftSide)
	. if $increment(currentAlias)
	. set rightSide="("_$$generateSubQuery("limited")_")"
	. if $increment(currentAlias,-1)
	. set aliasDotColumn=""
	. for i=1:1  do  quit:($find(aliasDotColumn,"alias"_aliasNum)'=0)
	. . set aliasDotColumn=$piece(rightSide," ",i)
	. set rightColumn=$piece(aliasDotColumn,".",2)
	.
	. new i
	. set holder=""
	. for i=1:1  do  quit:($find(holder,"FROM")'=0)
	. . set holder=$piece(rightSide," ",i)
	. set rightTable=$piece(rightSide," ",i+1)
	.
	. set rightType=$$returnColumnType(rightTable,rightColumn)
	.
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set leftSide=$$chooseColumn(table)
	. . set leftType=$$returnColumnType(table,leftSide)
	.
	. set leftSide=table_"."_leftSide
	.
	. if (i'=15) do
	. . set result=result_leftSide_" "_$$comparisonOperators_" "_word_" "_rightSide
	. else  set result=""

	if (randInt=10) do
	. new toCompare
	. set toCompare=$random(4)+1
	. set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","randomNumbers","numbers","FALSE",toCompare)

	if (randInt=11) do
	. new leftCaseArg,rightCaseArg,i,toCompare
	. set leftColumn=$$chooseColumn(table)
	.
	. set leftType=$$returnColumnType(table,leftColumn)
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set rightColumn=$$chooseColumn(table)
	. . set rightType=$$returnColumnType(table,rightColumn)
	.
	. set leftCaseArg=table_"."_leftColumn
	. if ($random(2))  set leftCaseArg=$$chooseEntry(table,leftColumn)
	. set rightCaseArg=table_"."_rightColumn
	. if ($random(2))  set rightCaseArg=$$chooseEntry(table,rightColumn)
	.
	. if (i'=15) do
	. . ; #FUTURE_TODO: The following variable "r" is magic, it is not populated, but when
	. . ;               the code is ran it contains a table.column, this needs more looking into
	. . ;               but for the time being it works, and it works properly so it will be left alone
	. . set toCompare=table_"."_$$chooseColumn(table)
	. . set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","lrComparison","columns","FALSE",toCompare)
	. else  set result=""

	; #FUTURE_TODO: Add more complexity here (boolean operators mostly)
	;               WHERE NOT booleanColumn, WHERE booleanColumn=TRUE/FALSE
	; Currently disabled until issue 346 is resolved
	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	if (randInt=999) do
	. set type=""
	. for  do  quit:($find(type,"BOOLEAN")'=0)
	. . set chosenColumn=$$chooseColumn(table)
	. . set type=$$returnColumnType(table,chosenColumn)
	. . ;if (type="BOOLEAN")  set result=result_chosenColumn
	. set result=result_chosenColumn

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	; #FUTURE_TODO: Maybe combine this with WHERE clause version #0 (randInt=0)
	; Disabled until issues 346,353 are resolved, also isn't yet finished
	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	if (randInt=999) do
	. new operator,leftSide,rightSide
	. set operator=$$comparisonOperators
	. set chosenColumn=$$chooseColumn(table)
	. set leftSide=""
	. set rightSide=""
	. if ($random(2))  set leftSide=chosenColumn
	. else  set leftSide=$$chooseEntry(table,chosenColumn)
	. if ($random(2))  set rightSide=chosenColumn
	. else  set rightSide=$$chooseEntry(table,chosenColumn)
	. set result=result_"(("_leftSide_" "_operator_" "_rightSide_") = "_$$tf_")"

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#group%20by%20clause
; This function returns the GROUP BY clause. It pulls the necessary values from the selectListLVN.
groupbyClause()
	new result,randInt,i,holder
	set result=" GROUP BY "

	set holder=" "
	for  quit:(holder="")  do
	. set holder=$order(selectListLVN(holder))
	. if (holder'="*")  set result=result_holder_", "

	; strip off the ", , " at the end of result
	if ($extract(result,$length(result)-3,$length(result))=", , ")  set result=$extract(result,0,$length(result)-3)

	set result=$extract(result,1,$length(result)-1)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#having%20clause
; #FUTURE_TODO: integrate aggregate functions into all of the below if blocks
; This function returns a HAVING clause for either a subquery or the parent query based on which clauseType is provided.
; "query" and "subquery" are valid clauseTypes
; If the HAVING Clause is to appear in the greater query (most external level) run with "query"
; If the HAVING Clause is to appear within a subquery run with "subquery"
havingClause(clauseType)
	if (clauseType="query")
	new result,randInt,holder,count,i,randValue,table,count
	set result=" HAVING "

	set holder=" "
	set count=-1
	for  quit:(holder="")  do
	. set holder=$order(selectListLVN(holder))
	. if $increment(count)

	if (clauseType="query") do
	. for  do  quit:($find(holder,"alias")=0)
	. . set holder=$order(selectListLVN(holder))
	. set table=$piece(holder,".",1)

	if (clauseType="subquery") do
	. set holder=""
	. for i=1:1  do  quit:($find(holder,"FROM")'=0)
	. . set holder=$piece(innerQuery," ",i)
	. set table=$piece(innerQuery," ",i+1)

	set randInt=$random(12) ; 0-11

	if ((randInt=5)&(existsInHavingExists="TRUE")) set randInt=1

	if (randInt=0) do
	. new loopCount,opened,i,leftSide,rightSide,notString,rightRand,leftRand,chosenColumn
	. set loopCount=$random(3)+1 ; 1-3
	. set opened="FALSE"
	. for i=1:1:loopCount do
	. . set leftSide=""
	. . set rightSide=""
	. . ; #FUTURE_TODO: Figure out way to identify type and match aggregate functions, and
	. . ;               then change below $random statements to $random(3)
	. . set leftRand=$random(2)+1
	. . set rightRand=$random(2)+1
	. . set chosenColumn=$$havingChooseColumn(count)
	. . if ((leftRand=0)&(clauseType="query")) set leftSide=$$returnAggregateFunction(table,chosenColumn,"HAVING")
	. . if ((leftRand=0)&(clauseType="subquery")) set leftSide=$$returnAggregateFunction(table,chosenColumn,"HAVING-SUBQUERY")
	. . if ((leftRand=1)&(clauseType="query"))  set leftSide=table_"."_chosenColumn
	. . if ((leftRand=1)&(clauseType="subquery"))  set leftSide="alias"_aliasNum_"."_chosenColumn
	. . if (leftRand=2)  set leftSide=$$chooseEntry(table,chosenColumn)
	. .
	. . if ((rightRand=0)&(clauseType="query"))  set rightSide=$$returnAggregateFunction(table,chosenColumn,"HAVING")
	. . if ((rightRand=0)&(clauseType="subquery"))  set rightSide=$$returnAggregateFunction(table,chosenColumn,"HAVING-SUBQUERY")
	. . if ((rightRand=1)&(clauseType="query"))  set rightSide=table_"."_chosenColumn
	. . if ((rightRand=1)&(clauseType="subquery"))  set rightSide="alias"_aliasNum_"."_chosenColumn
	. . if (rightRand=2)  set rightSide=$$chooseEntry(table,chosenColumn)
	. .
	. . set notString=""
	. . if $random(2) set notString="NOT "
	. .
	. . ; First portion of WHERE, no AND or OR
	. . if (i=1) set result=result_"("_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . ; Following portions of WHERE, with AND or OR separators
	. . if (i'=1) do
	. . . set result=result_" "_$$booleanOperator_" "
	. . . if (($random(2))&(opened="FALSE"))  do
	. . . . set result=result_"("
	. . . . set opened="TRUE"
	. . . set result=result_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . . if (($random(2))&(opened="TRUE"))  do
	. . . . set result=result_")"
	. . . . set opened="FALSE"
	. . if (i=loopCount)  set result=result_")"
	. . if ((i=loopCount)&(opened="TRUE"))  set result=result_")"

	if (randInt=1) do
	. new type,chosenColumn,plusMinus,i
	. set type=""
	. for i=1:1:15  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$havingChooseColumn(count)
	. . if (chosenColumn'="") set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. if ((i'=15)&(clauseType="query")) set result=result_"(("_table_"."_chosenColumn_plusMinus_$$chooseEntry(table,chosenColumn)_")="_$$chooseEntry(table,chosenColumn)_")"
	. if ((i'=15)&(clauseType="subquery")) set result=result_"(("_"alias"_aliasNum_"."_chosenColumn_plusMinus_$$chooseEntry(table,chosenColumn)_")="_$$chooseEntry(table,chosenColumn)_")"
	. if (i=15)  set randInt=2

	if (randInt=2) do
	. ; Left side of this expression will always be forced to be a varchar,
	. ; the right side of this expression can be either varchar or integer
	. ; This is done as PostgreSQL requires at least one side of a string concatenation
	. ; to be a string/varchar and the other to not be.
	. new leftSide,rightSide,type,chosenColumn1,chosenColumn2,entry1,entry2
	.
	. set leftSide=""
	. set rightSide=""
	.
	. set chosenColumn1=""
	. set type=""
	. for i=1:1:15  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn1=$$havingChooseColumn(count)
	. . set type=$$returnColumnType(table,chosenColumn1)
	. set entry1=$$chooseEntry(table,chosenColumn1)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. if (clauseType="query") set leftSide=table_"."_chosenColumn1
	. if (clauseType="subquery") set leftSide="alias"_aliasNum_"."_chosenColumn1
	. if $random(2)  set leftSide=$$chooseEntry(table,chosenColumn1)
	.
	. set chosenColumn2=$$havingChooseColumn(count)
	. set entry2=$$chooseEntry(table,chosenColumn2)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. if (clauseType="query") set rightSide=table_"."_chosenColumn2
	. if (clauseType="subquery") set rightSide="alias"_aliasNum_"."_chosenColumn2
	. if $random(2)  set rightSide=$$chooseEntry(table,chosenColumn2)
	.
	. if (i'=15) set result=result_"(("_leftSide_"||"_rightSide_")"_$$comparisonOperators_"'"_entry1_entry2_"')"
	. else  set randInt=3

	if (randInt=3) do
	. new type,chosenColumn,plusMinus,plusMinus2,i
	. set type=""
	. for i=1:1:15  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$havingChooseColumn(count)
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	.
	. if ((i'=15)&(clauseType="query")) set result=result_"("_table_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_"))"
	. if ((i'=15)&(clauseType="subquery")) set result=result_"("_"alias"_aliasNum_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_"))"
	. else  set randInt=4

	if (randInt=4) do
	. new type,chosenColumn,beginning,plusMinus,end,aOperator
	.
	. set type=""
	. for i=1:1:15  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$havingChooseColumn(count)
	. . set type=$$returnColumnType(table,chosenColumn)
	.
	. set beginning="("_$$chooseEntry(table,chosenColumn)_")"
	.
	. new plusMinus
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	.
	. set endEntry=$$chooseEntry(table,chosenColumn)
	.
	. ; Checks if the selected operator is division/modulo and then checks if the
	. ; divisor is 0, if it is 0, it then forces it to a non zero number (1)
	. set aOperator=$$arithmeticOperator
	. if (((aOperator="/")!(aOperator="%"))&(endEntry=0))  set endEntry=1
	.
	. set end=plusMinus_"("_plusMinus_endEntry_")"
	.
	. if ((i'=15)&(clauseType="query")) set result=result_"(("_beginning_" "_aOperator_" "_end_") "_$$comparisonOperators_" "_table_"."_chosenColumn_")"
	. if ((i'=15)&(clauseType="subquery")) set result=result_"(("_beginning_" "_aOperator_" "_end_") "_$$comparisonOperators_" "_"alias"_aliasNum_"."_chosenColumn_")"
	. else  set randInt=5

	if (randInt=5) do
	. set existsInHavingExists="TRUE"
	. new notString,alias
	. set notString=""
	. if $random(2) set notString="NOT "
	. if $increment(currentAlias)
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("full")_")"
	. if $increment(currentAlias,-1)

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$havingChooseColumn(count)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. set limit=($random($$maxIndex(table)-1)+1)
	. if (limit>8)  set limit=8
	. for i=1:1:limit do
	. . set entryList=entryList_$$chooseEntry(table,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. if (clauseType="query") set result=result_table_"."_chosenColumn_" "_notString_"IN ("_entryList_")"
	. if (clauseType="subquery") set result=result_"alias"_aliasNum_"."_chosenColumn_" "_notString_"IN ("_entryList_")"

	if (randInt=7) do
	. new chosenColumn
	. set chosenColumn=$$havingChooseColumn(count)
	. if (clauseType="query") set result=result_table_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)
	. if (clauseType="subquery") set result=result_"alias"_aliasNum_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	if (randInt=8) do
	. new chosenColumn,string,i,charCount
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for i=1:1:15  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$havingChooseColumn(count)
	. . set type=$$returnColumnType(table,chosenColumn)
	. set string=""
	. set desiredStringLength=$random(7)+1 ; 1-7
	. for charCount=1:1:desiredStringLength do
	. . set randValue=$random(9) ; 0-8
	. . if ((randValue=0)!(randValue=1)) set string=string_$char($random(27)+64) ; @,A-Z
	. . if ((randValue=2)!(randValue=3)) set string=string_$char($random(26)+97) ; a-z
	. . if (randValue=4) set string=string_$random(10) ; 0-9
	. . if (randValue=5) set string=string_"#"
	. . if (randValue=6) set string=string_"$"
	. . if (randValue=7) set string=string_"%"
	. . if (randValue=8) set string=string_"_"
	. if ((i'=15)&(clauseType="query")) set result=result_table_"."_chosenColumn_" LIKE '"_string_"'"
	. if ((i'=15)&(clauseType="subquery")) set result=result_"alias"_aliasNum_"."_chosenColumn_" LIKE '"_string_"'"
	. else  set randInt=9

	if (randInt=9) do
	. new randInt,word,leftSide,rightSide,chosenColumn,entryList,limit,rightType,i
	. set word=$$aas
	. set leftSide=$$havingChooseColumn(count)
	. set leftType=$$returnColumnType(table,leftSide)
	. if $increment(currentAlias)
	. set rightSide="("_$$generateSubQuery("limited")_")"
	. if $increment(currentAlias,-1)
	. ;if $increment(aliasNum,-1)
	. set aliasDotColumn=""
	. for i=1:1  do  quit:($find(aliasDotColumn,"alias"_aliasNum)'=0)
	. . set aliasDotColumn=$piece(rightSide," ",i)
	. set rightColumn=$piece(aliasDotColumn,".",2)
	.
	. new i
	. set holder=""
	. for i=1:1  do  quit:($find(holder,"FROM")'=0)
	. . set holder=$piece(rightSide," ",i)
	. set rightTable=$piece(rightSide," ",i+1)
	.
	. set rightType=$$returnColumnType(rightTable,rightColumn)
	.
	. if $increment(aliasNum,-1)
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set leftSide=$$havingChooseColumn(count)
	. . set leftType=$$returnColumnType(table,leftSide)
	.
	. if (clauseType="query") set leftSide=table_"."_leftSide
	. if (clauseType="subquery") set leftSide="alias"_aliasNum_"."_leftSide
	. if $increment(aliasNum)
	.
	. if (i'=15) set result=result_leftSide_" "_$$comparisonOperators_" "_word_" "_rightSide
	. else  set result=""

	if (randInt=10) do
	. new toCompare
	. set toCompare=$random(4)+1
	. if (clauseType="subquery")  set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","randomNumbers","numbers","TRUE",toCompare)
	. if (clauseType="query")  set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","randomNumbers","numbers","FALSE",toCompare)

	if (randInt=11) do
	. new leftCaseArg,rightCaseArg,i,toCompare
	. set leftColumn=$$chooseColumn(table)
	.
	. set leftType=$$returnColumnType(table,leftColumn)
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set rightColumn=$$chooseColumn(table)
	. . set rightType=$$returnColumnType(table,rightColumn)
	.
	. if (clauseType="query") set leftCaseArg=table_"."_leftColumn
	. if (clauseType="subquery") set leftCaseArg="alias"_aliasNum_"."_leftColumn
	. if ($random(2))  set leftCaseArg=$$chooseEntry(table,leftColumn)
	. if (clauseType="query") set rightCaseArg=table_"."_rightColumn
	. if (clauseType="subquery") set rightCaseArg="alias"_aliasNum_"."_rightColumn
	. if ($random(2))  set rightCaseArg=$$chooseEntry(table,rightColumn)
	.
	. if (i'=15) do
	. . if (clauseType="query") do
	. . . set toCompare=table_"."_$$chooseColumn(table)
	. . . set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","lrComparison","columns","FALSE",toCompare)
	. . if (clauseType="subquery") do
	. . . set toCompare="alias"_aliasNum_"."_$$chooseColumn(table)
	. . . set innerTable=table
	. . . set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","lrComparison","columns","TRUE",toCompare)
	. else  set result=""

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#order%20by%20clause
; This function returns an ORDER BY clause for either a subquery or the parent query.
; It takes in an alias number (aNum) as a paremeter.
; It also takes in a location value as a parameter.
; If the ORDER BY Clause is to appear in the parent query run with "QUERY"
; If the ORDER BY Clause is to appear within a subquery run with "SUBQUERY"
orderbyClause(aNum,location)
	new result,holder
	set result=" ORDER BY "

	; Following if statement checks for DISTINCT in the parent query as the
	; presence of a DISTINCT qualifier requires the ORDER BY to contain only
	; things that also occur in the SELECT LIST
	if ($data(quantiferLVN("alias"_aNum))'=0)  do
	. if (($random(2))!(quantiferLVN("alias"_aNum)="DISTINCT "))  do
	. . set holder=" "
	. . for  quit:(holder="")  do
	. . . set holder=$order(selectListLVN(holder))
	. . . if (holder'="*")  set result=result_holder_", "
	. .
	. . ; strip off the ", , " at the end of result
	. . if ($extract(result,$length(result)-3,$length(result))=", , ")  set result=$extract(result,0,$length(result)-4)
	. else  do
	. . ; #FUTURE_TODO: There is no basis for determinig whether an ORDER BY occurs within a subquery
	. . ;               or not, so eventually add the fourth parameter to the below call to
	. . ;               returnCaseFunction when this logic is implemented or needed
	. . if (location="QUERY") do
	. . . set toCompare=table_"."_$$chooseColumn(table)
	. . . set result=result_$$returnCaseFunction("ORDER BY","randomNumbers","columns","FALSE",toCompare)
	. . if (location="SUBQUERY") do
	. . . set subqueryTable=$extract(innerQuery,$find(innerQuery,"FROM "),$find(innerQuery," "))
	. . . set toCompare="alias"_aliasNum_"."_$$chooseColumn(subqueryTable)
	. . . set result=result_$$returnCaseFunction("ORDER BY","randomNumbers","columns","TRUE",toCompare)
	else  set result=""

	quit result

; Cannot find the link for this
; This function returns a LIMIT Clause, with a number between 1 and the amount of elements in the table.
limitClause()
	new result,holder,max,a
	set result=" LIMIT "
	set max=$$maxIndex(table)
	set result=result_($random(max-1)+1)

	quit result

; Cannot find a single link that defines all of the Join clause, just the smaller portions of it
; This function returns the JOIN clause. It only works for the parent query.
; This function conatians the logic for the selecting of a JOIN type, what is actually being joined, and the ON clause.
joinClause()
	new result,chosenTable1,chosenColumn1,chosenColumn2,chosenTable2,type1,joinType,operator,fullString,loopCount
	set result=""

	set joinType=$$joinTypes

	if (outerJoinsAllowed="FALSE")  set joinType="INNER"

	; Randomly tests FULL OUTER JOIN, as compared to just OUTER JOIN
	set fullString=" "
	if (($random(2))&(joinType="OUTER"))  set fullString=" FULL "

	set result=result_fullString_joinType_" JOIN "

	set chosenTable1=$order(tableColumn(1)) ; essentially table1
	set chosenColumn1=$$chooseColumnBasedOffTable(chosenTable1)
	set type1=$$returnColumnType(chosenTable1,chosenColumn1)
	set chosenEntry1=$$chooseEntry(chosenTable1,chosenColumn1)

	set randInt=$random(2)
	set chosenTable2=""
	set chosenColumn2=""
	set chosenEntry2=""
	set aliasNum1More=aliasNum+1
	set asPiece=" AS alias"_aliasNum1More

	if (randInt=0)  do
	. new type2,i,limiter
	. set chosenTable2=chosenTable1
	. for  quit:(chosenTable2'=chosenTable1)  do
	. . set chosenTable2=$$chooseTable ; essentially table2
	. set dontJoin(chosenTable2)=""
	.
	. set chosenColumn2=""
	. set type2=""
	. ; After 15 attempts at finding two compatible columns (same type), select a new chosenTable2
	. ; repeat this 2 more times and if this fails to produce a column that is valid, then delete
	. ; the join portion of the query
	. set limiter=1
	. for i=1:1:16  quit:(($piece(type2,"(")=$piece(type1,"("))!(limiter=3))  do
	. . if (i=15)  do
	. . . set i=1
	. . . if $increment(limiter)
	. . . set chosenTable2=chosenTable1
	. . . for  quit:((chosenTable2'=chosenTable1)&($data(chosenTable2)'=0))  do
	. . . . set chosenTable2=$$chooseTable ; essentially table2
	. . . set dontJoin(chosenTable2)=""
	. . set chosenColumn2=$$chooseColumnBasedOffTable(chosenTable2)
	. . set type2=$$returnColumnType(chosenTable2,chosenColumn2)
	. set chosenEntry2=$$chooseEntry(chosenTable2,chosenColumn2)
	.
	. if (limiter'=3)  do
	. . set result=result_chosenTable2_asPiece
	. . set chosenTable2="alias"_aliasNum
	. if (limiter=3)  set result=""

	if (randInt=1)  do
	. new subquery,i
	. set chosenEntry2=""
	. if $increment(currentAlias)
	. set subquery=$$generateSubQuery("full")
	. if $increment(currentAlias,-1)
	. set randFromInnerQuerySLLVN=$random(innerQuerySLLVN)
	. for i=0:1:randFromInnerQuerySLLVN  do
	. . set chosenColumn2=$order(innerQuerySLLVN("alias"_aliasNum,""))
	. set chosenTable2=innerQuerySLLVN("alias"_aliasNum,chosenColumn2)
	. set type2=$$returnColumnType(chosenTable2,chosenColumn2)
	.
	. for i=1:1:15  quit:($piece(type2,"(")=$piece(type1,"("))  do
	. . set chosenColumn1=$$chooseColumnBasedOffTable(chosenTable1)
	. . set type1=$$returnColumnType(chosenTable1,chosenColumn1)
	.	set chosenEntry1=$$chooseEntry(chosenTable1,chosenColumn1)
	.
	. if ($find(subquery,"EXISTS (")'=0)  do
	. . set aliasNum1Less=aliasNum-1
	. . set chosenTable2="alias"_aliasNum1Less
	. . for i=0:1:randFromInnerQuerySLLVN  do
	. . . set chosenColumn2=$order(innerQuerySLLVN("alias"_aliasNum1Less,""))
	. else  set chosenTable2="alias"_aliasNum
	.
	. if (i'=15)  set result=result_"("_subquery_")"_asPiece
	. if (i=15)  set result=""

	; #FUTURE_TODO: Implement CASE statements into ON clause like in below example
	;               EXAMPLE:https://dev.to/gschro/conditional-column-join-in-sql-5g03
	new i
	set onClause=""
	set loopCount=$random(3)+1
	set opened="FALSE"
	if ((result'="")&(joinType'="CROSS")&(joinType'="NATURAL"))  do
	. for i=1:1:loopCount  do
	. . new rightRand
	. . set notString=""
	. . if $random(2) set notString=" NOT"
	. . if ($random(2))  set leftSide=chosenTable1_"."_chosenColumn1
	. . else  set leftSide=chosenEntry1
	. .
	. . set rightRand=$random(3) ; 0-2
	. . if ((rightRand=0)!(chosenEntry2="")) do
	. . . set alias=""
	. . . for aliasI=1:1  do  quit:(alias="AS")
	. . . . set alias=$piece(result," ",aliasI)
	. . . set alias=$piece(result," ",aliasI+1)
	. . .
	. . . if (randInt=0) do
	. . . . set tableInResult=""
	. . . . for tableI=1:1  do  quit:(tableInResult="JOIN")
	. . . . . set tableInResult=$piece(result," ",tableI)
	. . . . set tableInResult=$piece(result," ",tableI+1)
	. . . . set chosenColumn2=$$chooseColumn(tableInResult)
	. . . . set rightSide=alias_"."_chosenColumn2
	. . . . set rightType=$$returnColumnType(tableInResult,chosenColumn2)
	. . . if (randInt=1) do
	. . . . for rightSideI=1:1  do  quit:(($find(rightSide,alias)'=0)&($find(rightSide,")")=0))
	. . . . . set rightSide=$piece(result," ",rightSideI)
	. . . . if ($extract(rightSide,$length(rightSide))=",")  set rightSide=$extract(rightSide,$length(rightSide)-1)
	. . . . set tableInResult=""
	. . . . for tableI=1:1  do  quit:(tableInResult="FROM")
	. . . . . set tableInResult=$piece(result," ",tableI)
	. . . . set tableInResult=$piece(result," ",tableI+1)
	. . . . set columnModified=$piece(rightSide,".",2)
	. . . . if ($find(columnModified,",")'=0)  set columnModified=$extract(columnModified,0,$length(columnModified)-1)
	. . . .
	. . . . set rightType=$$returnColumnType(tableInResult,columnModified)
	. . .
	. . . set leftType=""
	. . . for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . . . set leftSide=$$chooseColumn(chosenTable1)
	. . . . set leftType=$$returnColumnType(chosenTable1,leftSide)
	. . .
	. . . set leftSide=$piece(fc," ",2)_"."_leftSide
	. . if ((rightRand=1)&(chosenEntry2'=""))  set rightSide=chosenEntry2
	. . if (rightRand=2) do
	. . . if $increment(currentAlias)
	. . . set rightSide=$$aas_" ("_$$generateSubQuery("limited")_")"
	. . . if $increment(currentAlias,-1)
	. . .
	. . . set aliasDotColumn=""
	. . . for i=1:1:20  do  quit:($find(aliasDotColumn,"alias"_aliasNum)'=0)
	. . . . set aliasDotColumn=$piece(rightSide," ",i)
	. . . set rightColumn=$piece(aliasDotColumn,".",2)
	. . .
	. . . new rightI
	. . . set holder=""
	. . . for rightI=1:1  do  quit:($find(holder,"FROM")'=0)
	. . . . set holder=$piece(rightSide," ",rightI)
	. . . set rightTable=$piece(rightSide," ",rightI+1)
	. . . set rightType=$$returnColumnType(rightTable,rightColumn)
	. . .
	. . . set leftType=""
	. . . for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . . . set leftSide=$$chooseColumn(chosenTable1)
	. . . . set leftType=$$returnColumnType(chosenTable1,leftSide)
	. . .
	. . . set leftSide=$piece(fc," ",2)_"."_leftSide
	. .
	. . set operator=$$comparisonOperators
	. .
	. . ; Must always compare with an "=" as PostgreSQL issues the following error when FULL join:
	. . ; FULL JOIN is only supported with merge-joinable or hash-joinable join conditions
	. . if (joinType="FULL")  set operator="="
	. .
	. . ; #FUTURE_TODO: If an OUTER JOIN is selected, the only valid/working operator is "=", remove this force setting when Issue 311 is resolved
	. . ;               Also see tests/fixtures/TOJ03.m line 103 and 114 for more information pertaining to this problem
	. . if ((joinType="LEFT")!(joinType="RIGHT")!(joinType="LEFT OUTER")!(joinType="RIGHT OUTER")) set operator="="  set outerJoinExists="TRUE"
	. .
	. . ; FULL JOINs require the operator to be "="
	. . if (fullString=" FULL ")  set operator="="
	. .
	. . ; #FUTURE_TODO: Remove this check and all associated code/variables (outerJoinExists)
	. . ;               from above checks, and from the "global" area at the top of this file
	. . ;               when issue 311 is resolved
	. . if (outerJoinExists="TRUE")  set operator="="
	. .
	. . if (($random(2))&(opened="FALSE"))  do
	. . . set onClause=onClause_"("
	. . . set opened="TRUE"
	. . set onClause=onClause_"("_leftSide_" "_operator_" "_rightSide_")"
	. . if (($random(2))&(opened="TRUE"))  do
	. . . set onClause=onClause_")"
	. . . set opened="FALSE"
	. . if (i<loopCount)  set onClause=onClause_" "_$$booleanOperator_notString_" "
	. ;set onClause=" ON ("_$extract(onClause,1,$length(onClause))_")" ;remove before MR if not needed
	. set onClause=" ON ("_onClause_")"
	. if (opened="TRUE")  set onClause=onClause_")"  set opened="FALSE"
	. if (i=15)  set onClause=""  set result=""

	quit result_onClause

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Supporting Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This function will randomly select a table from an LVN of all possible tables.
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
	quit tables($random(tables))

; This function will randomly select a column from an LVN of all possible columns, based off of the provided table parameter.
chooseColumn(table)
	if (initDone("chooseColumn")=0) do
	. new a,currentTable,column,columnCount,x,count
	. for a=0:1:GLOBALtotalTables-1 do
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
	new CC,selectedColumn
	if (table="") set table=$order(tableColumn(""))

	set CC=$SELECT($data(tableColumnCounts(table))=0:$$columnCounter(table),1:tableColumnCounts(table))

	set selectedColumn=$order(columns(table,$random(CC-1)+1,$random(CC-1)+1))
	set tableColumn(table,selectedColumn)=""
	quit selectedColumn

; This function is designed for the HAVING clause. It will randomly select a table from the selectListLVN, which is required for the HAVING clause.
havingChooseColumn(count)
	new holder,randValue
	set randValue=$random(count)+1
	set holder=""
	for iterator=1:1:randValue  do
	. set holder=$order(selectListLVN(holder))

	quit $piece(holder,".",2)

; This function returns the number of columns in the provided table. It is mainly used in other supporting functions.
columnCounter(currentTable)
	set column=""
	set columnCount=1
	set x=""
	for  set x=$order(sqlInfo(currentTable,x))  quit:x=""  if $increment(columnCount)
	quit columnCount

; This function is very similar in function to chooseColumn, but its logic is setup for use in a specific spot (JOIN clause).
chooseColumnBasedOffTable(table)
	set CC=$SELECT($data(tableColumnCounts(table))=0:$$columnCounter(table),1:tableColumnCounts(table))
	set selectedColumn=$order(columns(table,$random(CC-1)+1,$random(CC-1)+1))
	quit selectedColumn

; This function returns a randomly selected entry from a table and column pair, which both need to be passed in as parameters.
; This function is one of the most common spots for infinite loops, as it assumes that it is being handed a valid set of tableName and columnName.
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

	;if ' found in entry, replace it with '', both Octo and PSQL require this for things like
	; "Name's Store" or "Stop N' Shop"
	set position=$find(entry,"'")
	if (position'=0)  do
	. for  do  quit:(position=0)
	. . set firstPart=$extract(entry,0,position-2)
	. . set secondPart=$extract(entry,position,1000000)
	. . set entry=firstPart_"''"_secondPart
	. . set position=$find(entry,"'",position+2)

	; If selected entry is not of "INTEGER" or "NUMERIC" type, pre/append a single quote (')
	; This is done to match PSQL format rules, Octo does not care
	; Depending on how Booleans are to be specified, this might need to be changed
	if (($find(type,"INTEGER")=0)&($find(type,"NUMERIC")=0)) set entry="'"_entry_"'"

	quit entry

; This function returns the positional number/index of a column in the columns LVN. It is mainly used for other supporting functions.
returnColumnIndex(tableName,columnName)
	; Returns index of column
	set index=0
	for  quit:($order(columns(tableName,index,""))=columnName)  if $increment(index)
	quit index

; This function returns the stored type of whatever tableName and columnName that is provided to it.
; This function also is a common cause of infinite loops as it assumes that it is being given a valid set of tableName and columnName.
returnColumnType(tableName,columnName)
	set index=$$returnColumnIndex(tableName,columnName)
	quit $order(sqlInfo(tableName,index,columnName,""))

; This function returns the maximum value of items in the data LVN for a given table.
; Essentially this function returns the max possible integer value for items in a given tableName.
maxIndex(tableName)
	new holder,maxIndex,a,thing

	set holder=" "
	for maxIndex=1:1  do  quit:(holder="")
	. set a="^"_tableName_"("_maxIndex_")"
	. set holder=$order(data(a,""))

	set maxIndex=maxIndex-1
	if (maxIndex=0)  set maxIndex=2
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
	set toBeReturned=comparisonOperators($random(comparisonOperators))
	quit toBeReturned

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

booleanOperator()
	if (initDone("booleanOperator")=0) do
	. set initDone("booleanOperator")=1
	. set booleanOperator=-1
	. set booleanOperator($increment(booleanOperator))="AND"
	. set booleanOperator($increment(booleanOperator))="OR"
	. if $increment(booleanOperator)
	quit booleanOperator($random(booleanOperator))

joinTypes()
	if (initDone("joinTypes")=0) do
	. set initDone("joinTypes")=1
	. set joinTypes=-1
	. set joinTypes($increment(joinTypes))="LEFT"
	. set joinTypes($increment(joinTypes))="LEFT OUTER"
	. set joinTypes($increment(joinTypes))="RIGHT"
	. set joinTypes($increment(joinTypes))="RIGHT OUTER"
	. set joinTypes($increment(joinTypes))="INNER"
	. set joinTypes($increment(joinTypes))="CROSS"
	. set joinTypes($increment(joinTypes))="NATURAL"
	. if $increment(joinTypes)
	; #FUTURE_TODO: Remove following 3 lines, and uncomment 4th line when Issue 311 is resolved
	set joinType=joinTypes($random(joinTypes))
	if ($find(query,"DISTINCT")'=0) set joinType="INNER"
	quit joinType
	;quit joinTypes($random(joinTypes))

tf()
	if (initDone("tf")=0) do
	. set initDone("tf")=1
	. set tf=-1
	. set tf($increment(tf))="TRUE"
	. set tf($increment(tf))="FALSE"
	. if $increment(tf)
	quit tf($random(tf))

; The "aas" ancronym stands for ANY, ALL, and SOME. This function returns one
; of the three words, and was written to prevent unneccesary code duplication.
aas()
	if (initDone("aas")=0) do
	. set initDone("aas")=1
	. set aas=-1
	. set aas($increment(aas))="ANY"
	. set aas($increment(aas))="ALL"
	. set aas($increment(aas))="SOME"
	. if $increment(aas)
	quit aas($random(aas))

; #FUTURE_TODO: Handle nulls in aggregrate functions, worry about
;               this when outer joins are reimplemented
;               http://www.sqlsnippets.com/en/topic-12656.html
; Valid placement parameters are "SL" and "HAVING" and "HAVING-SUBQUERY"
; Passing "SL" (Select List) returns an aggregate function matching
; requirements for an aggregate in the select list
; Passing "SL-SUBQUERY" (Select List) returns an aggregate function matching
; requirements for an aggregate in the select list, which is in turn located in a subquery
; Passing "HAVING" returns an aggregate function matching
; requirements for an aggregate in the having clause
; Passing "HAVING-SUBQUERY" returns an aggregate function matching requirements
; for an aggregate in the having clause, which is in turn located in a subquery
returnAggregateFunction(table,column,placement)
	if (initDone("aggregrateFunctions")=0) do
	. set initDone("aggregrateFunctions")=1
	.
	. set aggFunctionInt=-1
	. set aggFunctionInt($increment(aggFunctionInt))="AVG"
	. set aggFunctionInt($increment(aggFunctionInt))="COUNT"
	. set aggFunctionInt($increment(aggFunctionInt))="MIN"
	. set aggFunctionInt($increment(aggFunctionInt))="MAX"
	. set aggFunctionInt($increment(aggFunctionInt))="SUM"
	. if $increment(aggFunctionInt)
	.
	. set aggFunction=-1
	. set aggFunction($increment(aggFunction))="COUNT"
	. set aggFunction($increment(aggFunction))="MIN"
	. set aggFunction($increment(aggFunction))="MAX"
	. if $increment(aggFunction)
	.
	. set aggModifier=-1
	. set aggModifier($increment(aggModifier))=""
	. set aggModifier($increment(aggModifier))="DISTINCT "
	. set aggModifier($increment(aggModifier))="ALL "
	. if $increment(aggModifier)
	new result,function

	set result=""
	set function=""

	if (placement="SL") do
	. if ($$returnColumnType(table,column)="INTEGER")  set function="AVG";set function=aggFunctionInt($random(aggFunctionInt))
	. else  set function=aggFunction($random(aggFunction))
	. set result=function_"("_aggModifier($random(aggModifier))_table_"."_column_")"

	if (placement="SL-SUBQUERY") do
	. if ($$returnColumnType(table,column)="INTEGER")  set function=aggFunctionInt($random(aggFunctionInt))
	. else  set function=aggFunction($random(aggFunction))
	. set result=function_"("_aggModifier($random(aggModifier))_"alias"_aliasNum_"."_column_")"

	if (placement="HAVING") do
	. if ($$returnColumnType(table,column)="INTEGER")  set function=aggFunctionInt($random(aggFunctionInt))
	. else  set function=aggFunction($random(aggFunction))
	. set result=function_"("_table_"."_column_")"

	if (placement="HAVING-SUBQUERY") do
	. set function=aggFunction($random(aggFunction))
	. set result=function_"("_"alias"_aliasNum_"."_column_")"

	quit result

; #FUTURE_TODO: Eventually increase the depth that CASEs can occur, currently a single CASE can be nested inside of a single CASE
; Valid "location" parameters are "SELECT LIST", "ORDER BY", "WHERE", "HAVING"
; "SELECT LIST" generates a CASE statement that matches the requirements for SELECT LIST (i.e. an alias)
; "ORDER BY" generates a CASE statement that matches the requirements for ORDER BY
; "WHERE" generates a CASE statement that matches the requirements for WHERE
; "HAVING" generates a CASE statement that matches the requirements for HAVING
; Currently "ORDER BY", "WHERE", and "HAVING" all follow very similar paths through the code
;
; Valid "conditionType" parameters are "randomNumbers", "lrComparison"
; "randomNumbers" simply compares two randomly selected integers between 0 and 4
; "lrComparison" compares two randomly selected items named "leftCaseArg" and "rightCaseArg",
;     which need to be declared right before the call to this function
;
; Valid "resultType" parameters are "numbers", "columns"
; "numbers" set the resulting value to the value of "i" (the loop control variable)
; "columns" set the resulting value to a random column selected from a provided table
;     named "table" which needs to be declared right before the call to this function
;
; Valid "subqueryBoolean" parameters are "TRUE" and "FALSE", run with "TRUE" if CASE
; statement is to occur within a subquery, or "FALSE" if CASE statement is to occur
; if
; in the parent query
returnCaseFunction(location,conditionType,resultType,subqueryBoolean,toCompare)
	new result,whenCount,i,condition,r,limiter
	if (caseFunctionExists="FALSE")  set result="CASE"_$char(10)
	if (caseFunctionExists="TRUE")  set result="  CASE"_$char(10)

	set whenCount=$random(4)+1 ; 1-5
	set limiter=1

	; Limit the amount of nesting that can be done with CASEs
	for i=1:1:whenCount  do
	. set condition=""
	. if (conditionType="randomNumbers")  set condition="("_$random(5)_$$comparisonOperators_$random(5)_")"
	. if (conditionType="lrComparison")  set condition="("_leftCaseArg_$$comparisonOperators_rightCaseArg_")"
	.
	. set r=""
	. if (resultType="numbers")  set r=i
	. if (resultType="columns")&(subqueryBoolean="FALSE") do
	. . set mainType=$$returnColumnType(table,$piece(toCompare,".",2))
	. . set otherType=""
	. . for  do  quit:($piece(mainType,"(")=$piece(rType,"("))
	. . . set r=$$chooseColumn(table)
	. . . set rType=$$returnColumnType(table,r)
	. . set r=table_"."_r
	. if (resultType="columns")&(subqueryBoolean="TRUE") do
	. . if ($find($piece(toCompare,"."),"alias")'=0) do
	. . . set mainType=$$returnColumnType(innerTable,$piece(toCompare,".",2))
	. . else  do
	. . . set mainType=$$returnColumnType($piece(toCompare,"."),$piece(toCompare,".",2))
	. . set otherType=""
	. . for  do  quit:($piece(mainType,"(")=$piece(rType,"("))
	. . . ;for  do  quit:($data(caseLVN("alias"_aliasNum,location)=11))
	. . . set r=$$chooseColumn(innerTable)
	. . . set rType=$$returnColumnType(innerTable,r)
	. . . ;set caseLVN("alias"_aliasNum,location)=r
	. . set r="alias"_aliasNum_"."_r
	.
	. ; About 1/10 of the time nest another CASE into the current CASE
	. if ('($random(10))&(caseFunctionExists="FALSE")&(location'="WHERE"))  do
	. . set caseFunctionExists="TRUE"
	. . set result=result_"  WHEN "_condition_" THEN "_$char(10)_$$returnCaseFunction(location,conditionType,resultType,toCompare)
	. else  do
	. . if (caseFunctionExists="FALSE")  set result=result_"  WHEN "_condition_" THEN "_r_$char(10)
	. . if (caseFunctionExists="TRUE")  set result=result_"    WHEN "_condition_" THEN "_r_$char(10)
	.
	. if (limiter=15)  set result=""

	;if ($random(2))  do
	;. if $increment(i)
	;. if (caseFunctionExists="FALSE")  set result=result_"  ELSE "_r_$char(10)
	;. if (caseFunctionExists="TRUE")  set result=result_"    ELSE "_r_$char(10)
	; #FUTURE_TODO: Uncomment preceding if block and remove following three lines when issue 450 is
	;               resolved. "ELSE" currently cannot be optional until this issue is resolved.
	if $increment(i)
	if (caseFunctionExists="FALSE")  set result=result_"  ELSE "_r_$char(10)
	if (caseFunctionExists="TRUE")  set result=result_"    ELSE "_r_$char(10)

	if (location="SELECT LIST")  do
	. if (caseFunctionExists="FALSE")  if $increment(aliasNum)  set result=result_"END AS alias"_aliasNum
	. if (caseFunctionExists="TRUE")  set result=result_"  END"_$char(10)
	else  do
	. if (caseFunctionExists="FALSE")  set result=result_"END"
	. if (caseFunctionExists="TRUE")  set result=result_"  END"_$char(10)

	set caseFunctionExists="FALSE"

	quit result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This function is responsible for the actual generation of the parent query, and such contains most of the logic to do so.
generateQuery()
	new fc,i
	set selectListDepth=$random(GLOBALtotalTables)+1

	set query="SELECT "
	set query=query_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	set query=query_$$selectList(selectListDepth)
	set query=query_" "_fc
	if (($random(2))&(GLOBALtotalTables>1))  do
	. if ($random(2**5)=0)  set joinCount=5
	. if ($random(2**4)=0)  set joinCount=4
	. if ($random(2**3)=0)  set joinCount=3
	. if ($random(2**2)=0)  set joinCount=2
	. else  set joinCount=1
	.
	. ; Multiway JOINs are only currently supported with INNER JOIN,
	. ; following check enforces this behavior, remove check when Issue #311 is resolved
	. if (joinCount=1)  set outerJoinsAllowed="TRUE"
	.
	. for i=1:1:joinCount  set query=query_$$joinClause  if $increment(aliasNum)
	set query=query_$$tableExpression
	quit query_";"

; This function generates all of the subqueries for the various other functions throughout this program.
; Valid parameters are "full" and "limited"
; Passing "full" returns a very general query with minimal restrictions
; Passing "limited" returns a query that contains clauses that limit the
; query to only return a single row, and single column
generateSubQuery(subQueryType)
	new innerFC,alias
	if $increment(aliasNum)
	set alias="alias"_aliasNum

	merge tableColumnTemp=tableColumn
	merge selectListLVNTemp=selectListLVN
	new tableColumn,selectListLVN


	set allowGBH("alias"_aliasNum)="FALSE"
	if '($random(5))  set allowGBH("alias"_aliasNum)="TRUE"

	new innerQuery
	set innerQuery="SELECT "
	set innerQuery=innerQuery_$$setQuantifier
	set innerFC=$$fromClause ; fromClause needs to run before selectList
	if (subQueryType="full")  set innerQuery=innerQuery_$$innerSelectList(subQueryType,$random(3)+1,alias)
	if (subQueryType="limited")  set innerQuery=innerQuery_$$innerSelectList(subQueryType,0,alias)
	set innerQuery=innerQuery_" "_innerFC_" "_alias
	set innerQuery=innerQuery_$$innerTableExpression(subQueryType)

	new tableColumn,selectListLVN
	merge tableColumn=tableColumnTemp
	merge selectListLVN=selectListLVNTemp
	quit innerQuery

; #FUTURE_TODO: Try to make as many of these the same as their non-subquery counterparts
;               innerSelectList is almost the same as selectList (Different logic, but same idea)
;               innerTableExpression is almost the same as tableExpression
;               innerWhereClause is almost the same as whereClause
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Inner Specific Versions of Other Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
; This function returns an select list that matches requirements and utilizes the variables of subqueries.
innerSelectList(subQueryType,curDepth,alias)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""

	set toBeAdded=""
	; Qualifier notation (table.column), with the value of the alias variable instead of a table
	set toBeAdded=alias_"."_$$chooseColumn("")

	if ((subQueryType="full")&(allowGBH("alias"_aliasNum)="FALSE"))  do
	. for i=1:1:15  quit:($data(selectListLVN(toBeAdded))=0)  do
	. . set toBeAdded=alias_"."_$$chooseColumn("")
	.
	. if (i'=15)  do
	. . set selectListLVN(toBeAdded)=""
	. . set innerQuerySLLVN($piece(toBeAdded,"."),$piece(toBeAdded,".",2))=$piece(innerFC," ",2)
	. . if $increment(innerQuerySLLVN)
	. . set result=result_toBeAdded
	.
	. if (i=15)  set curDepth=0  set result=$extract(result,0,$length(result)-1)


	if ((subQueryType="full")&(allowGBH("alias"_aliasNum)="TRUE"))  do
	. new table,chosenColumn,agg,chosenColumn2,tc
	. ; #FUTURE_TODO: Remove following line when issues (both in Octo and in test_helpers)
	. ;               pertaining to aggregate functions are resolved
	. set curDepth=0
	.
	. set table=$order(tableColumn(1))
	. set chosenColumn=$$chooseColumn(table)
	. set agg=$$returnAggregateFunction(table,chosenColumn,"SL-SUBQUERY")
	.
	. for  do  quit:(chosenColumn'=chosenColumn2)
	. . set chosenColumn2=$$chooseColumn(table)
	. set tc="alias"_aliasNum_"."_chosenColumn2
	. set innerQuerySLLVN($piece(tc,"."),$piece(tc,".",2))=$piece(innerFC," ",2)
	. if $increment(innerQuerySLLVN)
	.
	. set toBeAdded=agg_", "_tc
	. set selectListLVN(tc)=""
	.
	. set result=result_toBeAdded

	if (subQueryType="limited")  do
	. set selectListLVN(toBeAdded)=""
	. set innerQuerySLLVN($piece(toBeAdded,"."),$piece(toBeAdded,".",2))=$piece(innerFC," ",2)
	. set result=result_toBeAdded

	; #FUTURE_TODO: Move the recursion logic down here
	if ((subQueryType="full")&(curDepth>0)&(allowGBH("alias"_aliasNum)="FALSE"))  do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$innerSelectList(subQueryType,curDepth,alias)

	; strip off the ", " at the end of result, only if ", " is present in result
	if ($extract(result,$length(result)-1,$length(result))=", ")  set result=$extract(result,0,$length(result)-2)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
; This function returns the table expressions that matches requirements and utilizes the variables of subqueries.
; It only contains code to return a subquery specific WHERE CLAUSE, and regular/general GROUP BY, HAVING, LIMIT, and ORDER BY clauses.
innerTableExpression(subQueryType)
	new innerResult
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen
	set innerResult=""

	if ($random(2))  set innerResult=innerResult_$$innerWhereClause
	if ((allowGBH("alias"_aliasNum)="TRUE")&(subQueryType="full"))  set innerResult=innerResult_$$groupbyClause
	if ((allowGBH("alias"_aliasNum)="TRUE")&(subQueryType="full"))  set innerResult=innerResult_$$havingClause("subquery")
	if (('$random(8))&(subQueryType="full"))  set innerResult=innerResult_$$orderbyClause(aliasNum,"SUBQUERY")_$$limitClause
	if (subQueryType="limited")  set innerResult=innerResult_$$orderbyClause(aliasNum,"SUBQUERY")_" LIMIT 1"

	quit innerResult

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
; This function returns a WHERE CLAUSE specific to subqueries. This is due to the fact that the WHERE clause in a subquery needed a lot more logic to properly run, and select/use proper elements within.
innerWhereClause()
	new result,randInt,i,x
	set result=" WHERE "
	set randInt=$random(12) ; 0-11 ; Increase this range as new versions of WHERE clauses are added

	; #FUTURE_TODO: Fix WHERE clause version 5 (randInt=5) of innerWhereClause and remove following
	;               check (currently disabled).
	; randInt=5 does not work when there is also a JOIN with subquery in the query.
	; This occurs as a result of the desired alias(#) not existing in innerQuerySLLVN
	; randInt=5 also does not work as a result of it creating the "infinite" EXISTS loop
	; EX: ... WHERE EXISTS subquery(... WHERE EXISTS subquery(... WHERE EXISTS subquery(... WHERE EXISTS)))
	for  quit:(randInt'=5)  set randInt=$random(9)

	set innerTable=$piece(innerFC," ",2)

	set type=""

	; When randInt=2 or 8 a VARCHAR type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 2 is string concatenation, which requires at least one VARCHAR in order to function
	; WHERE clause type 8 is LIKE with wildcards, this comparison can only occur on VARCHARs, not numeric/integer
	if ((randInt=2)!(randInt=8))  do
	. set x="sqlInfo("""_innerTable_""")"
	. for i=1:1  do  quit:($find(type,"VARCHAR")'=0)
	. . set x=$query(@x)
	. . set type=$qsubscript(x,4)
	. . if ($qsubscript(x,1)'=innerTable)  set randInt=0

	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	; When randInt=999 a BOOLEAN type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 999 is just "WHERE boolean-type-column"
	; Currently commented out until issue 346 is resolved
	;if (randInt=999)  do
	;. set x="sqlInfo("""_innerTable_""")"
	;. for i=1:1:15  do  quit:(($find(type,"BOOLEAN")'=0)!(x=""))
	;. . set x=$query(@x)
	;. . if (x'="")  set type=$qsubscript(x,4)
	;. . if ((x'="")&($qsubscript(x,1)'=innerTable))  set randInt=0
	;. if ((i=15)!(x=""))  set randInt=0

	if (randInt=0) do
	. new loopCount,i,leftSide,rightSide,notString,chosenColumn,opened
	. set loopCount=$random(3)+1 ; 1-3
	. set opened="FALSE"
	. for i=1:1:loopCount do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set leftSide=""
	. . set rightSide=""
	. . if $random(2)  set leftSide="alias"_aliasNum_"."_chosenColumn
	. . else  set leftSide=$$chooseEntry(innerTable,chosenColumn)
	. . if $random(2)  set rightSide="alias"_aliasNum_"."_chosenColumn
	. . else  set rightSide=$$chooseEntry(innerTable,chosenColumn)
	. . set notString=""
	. . if $random(2) set notString="NOT "
	. .
	. . ; First portion of WHERE, no AND or OR
	. . if (i=1) set result=result_"("_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . ; Following portions of WHERE, with AND or OR separators
	. . if (i'=1) do
	. . . set result=result_" "_$$booleanOperator_" "
	. . . if (($random(2))&(opened="FALSE"))  do
	. . . . set result=result_"("
	. . . . set opened="TRUE"
	. . . set result=result_notString_"("_leftSide_" "_$$comparisonOperators_" "_rightSide_")"
	. . . if (($random(2))&(opened="TRUE"))  do
	. . . . set result=result_")"
	. . . . set opened="FALSE"
	. . if (i=loopCount)  set result=result_")"
	. . if ((i=loopCount)&(opened="TRUE"))  set result=result_")"

	if (randInt=1) do
	. new type,chosenColumn,plusMinus
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set result=result_"(("_"alias"_aliasNum_"."_chosenColumn_plusMinus_$$chooseEntry(innerTable,chosenColumn)_")="_$$chooseEntry(innerTable,chosenColumn)_")"

	if (randInt=2) do
	. new leftSide,rightSide,type,entry1,entry2
	. ; Left side of this expression will always be forced to be a varchar,
	. ; the right side of this expression can be either varchar or integer
	. ; This is done as PostgreSQL requires at least one side of a string concatenation
	. ; to be a string/varchar and the other to not be.
	. new leftSide,rightSide,type,chosenColumn1,chosenColumn2,entry1,entry2
	.
	. set leftSide=""
	. set rightSide=""
	.
	. set chosenColumn1=""
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn1=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn1)
	. set entry1=$$chooseEntry(innerTable,chosenColumn1)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. set leftSide="alias"_aliasNum_"."_chosenColumn1
	. if $random(2)  set leftSide=$$chooseEntry(innerTable,chosenColumn1)
	.
	. set chosenColumn2=$$chooseColumn("")
	. set entry2=$$chooseEntry(innerTable,chosenColumn2)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. set rightSide="alias"_aliasNum_"."_chosenColumn2
	. if $random(2)  set rightSide=$$chooseEntry(innerTable,chosenColumn2)
	.
	. set result=result_"(("_leftSide_"||"_rightSide_")"_$$comparisonOperators_"'"_entry1_entry2_"')"

	if (randInt=3) do
	. new type,chosenColumn,plusMinus,plusMinus2
	. ; ... WHERE customer_id=-(-3)
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	. set result=result_"("_"alias"_aliasNum_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(innerTable,chosenColumn)_"))"

	if (randInt=4) do
	. new type,chosenColumn,beginning,plusMinus,end,aOperator
	.
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn)
	.
	. set beginning="("_$$chooseEntry(innerTable,chosenColumn)_")"
	.
	. new plusMinus
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	.
	. set endEntry=$$chooseEntry(innerTable,chosenColumn)
	.
	. ; Checks if the selected operator is division/modulo and then checks if the
	. ; divisor is 0, if it is 0, it then forces it to a non zero number (1)
	. set aOperator=$$arithmeticOperator
	. if (((aOperator="/")!(aOperator="%"))&(endEntry=0))  set endEntry=1
	.
	. set end=plusMinus_"("_plusMinus_endEntry_")"
	.
	. set result=result_"(("_beginning_" "_aOperator_" "_end_") "_$$comparisonOperators_" "_"alias"_aliasNum_"."_chosenColumn_")"

	if (randInt=5) do
	. new notString,alias
	. ; ... WHERE EXISTS (SELECT ... query)
	. set notString=""
	. if $random(2) set notString="NOT "
	. if $increment(currentAlias)
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("full")_")"
	. if $increment(currentAlias,-1)

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$chooseColumn(innerTable)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. set limit=($random($$maxIndex(innerTable)-1)+1)
	. if (limit>8)  set limit=8
	. for i=1:1:limit do
	. . set entryList=entryList_$$chooseEntry(innerTable,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" "_notString_"IN ("_entryList_")"

	if (randInt=7) do
	. new chosenColumn
	. set chosenColumn=$$chooseColumn(innerTable)
	. ; ... WHERE column BETWEEN entry1 and entry2
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" BETWEEN "_$$chooseEntry(innerTable,chosenColumn)_" AND "_$$chooseEntry(innerTable,chosenColumn)

	if (randInt=8) do
	. new chosenColumn,string,i,randInt
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn)
	. set string=""
	. set desiredStringLength=$random(7)+1 ; 1-7
	. for i=1:1:desiredStringLength do
	. . set randInt=$random(9) ; 0-8
	. . if ((randInt=0)!(randInt=1)) set string=string_$char($random(27)+64) ; @,A-Z
	. . if ((randInt=2)!(randInt=3)) set string=string_$char($random(26)+97) ; a-z
	. . if (randInt=4) set string=string_$random(10) ; 0-9
	. . if (randInt=5) set string=string_"#"
	. . if (randInt=6) set string=string_"$"
	. . if (randInt=7) set string=string_"%"
	. . if (randInt=8) set string=string_"_"
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" LIKE '"_string_"'"

	if (randInt=9) do
	. new randInt,word,leftSide,rightSide,chosenColumn,entryList,limit,rightType
	. set word=$$aas
	. set leftSide=$$chooseColumn(innerTable)
	. set leftType=$$returnColumnType(innerTable,leftSide)
	.
	. if $increment(currentAlias)
	. set rightSide="("_$$generateSubQuery("limited")_")"
	. if $increment(currentAlias,-1)
	. set innerTable=$piece(innerFC," ",2)
	.
	. ;if $increment(aliasNum,-1)
	. set aliasDotColumn=""
	. for i=1:1  do  quit:($find(aliasDotColumn,"alias"_aliasNum)'=0)
	. . set aliasDotColumn=$piece(rightSide," ",i)
	. set rightColumn=$piece(aliasDotColumn,".",2)
	.
	. new i
	. set holder=""
	. for i=1:1  do  quit:($find(holder,"FROM")'=0)
	. . set holder=$piece(rightSide," ",i)
	. set rightTable=$piece(rightSide," ",i+1)
	.
	. set rightType=$$returnColumnType(rightTable,rightColumn)
	.
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set leftSide=$$chooseColumn(innerTable)
	. . set leftType=$$returnColumnType(innerTable,leftSide)
	.
	. if $increment(aliasNum,-1)
	. set leftSide="alias"_aliasNum_"."_leftSide
	.
	. if (i'=15) do
	. . set result=result_leftSide_" "_$$comparisonOperators_" "_word_" "_rightSide
	. else  set result=""

	if (randInt=10) do
	. new toCompare
	. set toCompare=$random(4)+1
	. set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","randomNumbers","numbers","TRUE",toCompare)

	if (randInt=11) do
	. new leftCaseArg,rightCaseArg,i,toCompare
	. set leftColumn=$$chooseColumn(innerTable)
	.
	. set leftType=$$returnColumnType(innerTable,leftColumn)
	. for i=1:1:15  do  quit:($piece(leftType,"(")=$piece(rightType,"("))
	. . set rightColumn=$$chooseColumn(innerTable)
	. . set rightType=$$returnColumnType(innerTable,rightColumn)
	.
	. set leftCaseArg="alias"_aliasNum_"."_leftColumn
	. if ($random(2))  set leftCaseArg=$$chooseEntry(innerTable,leftColumn)
	. set rightCaseArg="alias"_aliasNum_"."_rightColumn
	. if ($random(2))  set rightCaseArg=$$chooseEntry(innerTable,rightColumn)
	.
	. if (i'=15) do
	. . set toCompare="alias"_aliasNum_"."_$$chooseColumn(innerTable)
	. . set result=result_toCompare_" = "_$$returnCaseFunction("WHERE","lrComparison","columns","TRUE",toCompare)
	. else  set result=""

	; #FUTURE_TODO: Add more complexity here (boolean operators mostly)
	;               WHERE NOT booleanColumn, WHERE booleanColumn=TRUE/FALSE
	; Currently disabled until issue 346 is resolved
	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	if (randInt=999) do
	. set type=""
	. for  quit:($find(type,"BOOLEAN")'=0)  do
	. . set chosenColumn=$$chooseColumn(innerTable)
	. . set type=$$returnColumnType(innerTable,chosenColumn)
	. . ;if (type="BOOLEAN")  set result=result_chosenColumn
	. set result=result_chosenColumn

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	; #FUTURE_TODO: Maybe combine this with WHERE clause version #0 (randInt=0)
	; Disabled until issues 346 are resolved, also isn't yet finished
	; The comparison for randInt is set to 999 as to allow for new cases in the
	; WHERE clause to be added in numerical order, when reenabled change the 999
	; to whatever the next integer value would be in the series
	if (randInt=999) do
	. new operator,leftSide,rightSide
	. set operator=$$comparisonOperators
	. set chosenColumn=$$chooseColumn(innerTable)
	. set leftSide=""
	. set rightSide=""
	. if ($random(2))  set leftSide=chosenColumn
	. else  set leftSide=$$chooseEntry(innerTable,chosenColumn)
	. if ($random(2))  set rightSide=chosenColumn
	. else  set rightSide=$$chooseEntry(innerTable,chosenColumn)
	. set result=result_"(("_leftSide_" "_operator_" "_rightSide_") = "_$$tf_")"

	quit result
