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
	; #FUTURE_TODO: Implement subqueries into other places (Group By, Having, etc.). Example below
	;               SELECT n1.id from names n1 where n1.id * (select n2.id from names n2 where n2.id = n1.id % 3) = n1.id * 2;
	; #FUTURE_TODO: Make it so subqueries from the select list can be used in other places
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

	set initDone("setQuantifier")=0
	set initDone("chooseTable")=0
	set initDone("chooseColumn")=0
	set initDone("arithmeticOperator")=0
	set initDone("comparisonOperators")=0
	set initDone("booleanOperator")=0
	set initDone("joinTypes")=0
	set initDone("tf")=0
	set GLOBALtotalTables=0
	set joinNum=0
	set orderByExists="FALSE"
	set limitExists="FALSE"
	set outerJoinExists="FALSE"
	set outerJoinsAllowed="FALSE"

	set arguments=$zcmdline
	set sqlFile=$piece(arguments," ",1)
	if (sqlFile="") set sqlFile="customers.sql"
	set zwrFile=$piece(arguments," ",2)
	if (zwrFile="") set zwrFile="customers.zwr"
	set runCount=$piece(arguments," ",3)
	if (runCount="") set runCount=1
	set prefix=$piece(arguments," ",4)
	if (prefix="") set prefix="query"

	do readSQL($$findFile(sqlFile))
	do readZWR($$findFile(zwrFile))

	do checkForEmptyTables()

	new i
	for i=1:1:runCount do
	. set aliasNum=1
	. set fromNum=1
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
	. set orderByExists="FALSE"  set limitExists="FALSE"  set outerJoinExists="FALSE"  set joinNum=0
	. set outerJoinsAllowed="FALSE"

	quit

; #FUTURE_TODO: Look into using $piece functions instead of $extract and $find for parsers
;Parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
findFile(file)
	if ($zsearch(file)="") do
	. set file="../../tests/fixtures/"_file
	quit file

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
	. . . . set data(table,i2,firstData)=""
	. . . set holder=" "
	. . . if (i2'=1) do
	. . . . set holder=$piece(line(i),"|",i2)
	. . . . ;If there is a " in the string, remove it
	. . . . if (holder["""") set holder=$extract(holder,1,$find(holder,"""")-2)
	. . . . if (holder'="") set data(table,i2,holder)=""
	. . . set pKey=$extract(line(i),$find(line(i),"("),$find(line(i),")")-2)
	. . . set primaryKeys($extract(table,0,$find(table,"(")-2),pKey)=""
	quit

; If a table "exists" but has no data (such as namesWithAges in names db),
; then this function will remove it
checkForEmptyTables()
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
setQuantifier(curDepth)
	if (initDone("setQuantifier")=0) do
	. set initDone("setQuantifier")=1
	. set quantifier=-1
	. set quantifier($increment(quantifier))="DISTINCT "
	. set quantifier($increment(quantifier))="ALL "
	. set quantifier($increment(quantifier))=""
	. if $increment(quantifier)
	; #FUTURE_TODO: Just quit the random value when issue 311 is resolved
	set actualQuantifier=quantifier($random(quantifier))
	quit actualQuantifier

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
selectList(curDepth)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=$random(6) ; 0-5 ; #FUTURE_TODO Increase to 7 when issues #385 and #386 are resolved
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
	. set alias="alias"_aliasNum
	. set toBeAdded="("_$$generateSubQuery("limited")_") AS "_alias
	. set selectListLVN(alias)=""

	if (randInt=6)  do
	. set toBeAdded="*"
	. set selectListLVN(toBeAdded)=""

	set result=result_toBeAdded

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$selectList(curDepth)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
tableExpression()
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
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	; #FUTURE_TODO: Remove following line, and reenable following commented out line
	;               when Issue 311 is resolved
	; #FUTURE_TODO: Avoid ORDER BY clause selection if OUTER JOINs are used in the query (related to Issue 311).
	;               When 311 is resolved, the line containing `$find` calls below (to search for OUTER JOIN usages)
	;               can be removed and the line after it can instead be uncommented.
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$orderbyClause  set orderByExists="TRUE"
	;if ($random(2))  set result=result_$$orderbyClause  set orderByExists="TRUE"
	if '($random(8))  set result=result_$$limitClause  set limitExists="TRUE"

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#from%20clause
fromClause()
	new result,randInt,i,x,chosenTable
	;Constructs a FROM clause from a random number of table references.

	set result="FROM "

	; This is much harder than first thought, a variety of different issues
	; #FUTURE_TODO: This works from a functionality standpoint, the issue is that when a JOIN clause
	;               is also in the query, the tables in the fromClause overlap with those in the JOIN
	;               The tables in the fromClause are the same as those that are to be joined, which
	;               Octo and PostgreSQL both do not like. This can be resolved using aliases.
	;               This TODO requires a lot more work than previously thought, a large number of
	;               other functions will need to be rewritten in order to handle having a list of FROMS.
	;               Going into the future, these changes will likely need to be done in a seperate merge
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
whereClause()
	new result,randInt,i,x
	set result=" WHERE "
	set randInt=$random(9) ; 0-8 ; Increase this range as new versions of WHERE clauses are added

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

	; When randInt=9 a BOOLEAN type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 9 is just "WHERE boolean-type-column"
	; Currently commented out until issue 346 is resolved
	;if (randInt=9)  do
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
	. . set chosenColumn=$$chooseColumn("")
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
	. . set chosenColumn=$$chooseColumn("")
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
	. . set chosenColumn1=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn1)
	. set entry1=$$chooseEntry(table,chosenColumn1)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. set leftSide=table_"."_chosenColumn1
	. if $random(2)  set leftSide=$$chooseEntry(table,chosenColumn1)
	.
	. set chosenColumn2=$$chooseColumn("")
	. set entry2=$$chooseEntry(table,chosenColumn2)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. set rightSide=table_"."_chosenColumn2
	. if $random(2)  set rightSide=$$chooseEntry(table,chosenColumn2)
	.
	. set result=result_"(("_leftSide_"||"_rightSide_")"_$$comparisonOperators_"'"_entry1_entry2_"')"

	if (randInt=3) do
	. new type,chosenColumn,plusMinus,plusMinus2
	. ; ... WHERE customer_id=-(-3)
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	. set result=result_"("_table_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_"))"

	if (randInt=4) do
	. new type,chosenColumn,beginning,plusMinus,end,aOperator
	. ; ... WHERE customer_id=-(-3)
	. ; ... WHERE (math expression) arithmetic operator -(math expression)
	.
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
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
	. ; ... WHERE EXISTS (SELECT ... query)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("full")_")"

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$chooseColumn("")
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
	. set chosenColumn=$$chooseColumn("")
	. ; ... WHERE column BETWEEN entry1 and entry2
	. set result=result_table_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	if (randInt=8) do
	. new chosenColumn,string,i,randInt
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
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

	; #FUTURE_TODO: Add more complexity here (boolean operators mostly)
	;               WHERE NOT booleanColumn, WHERE booleanColumn=TRUE/FALSE
	; Currently disabled until issue 346 is resolved
	if (randInt=9) do
	. set type=""
	. for  quit:($find(type,"BOOLEAN")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. . ;if (type="BOOLEAN")  set result=result_chosenColumn
	. set result=result_chosenColumn

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	; #FUTURE_TODO: Maybe combine this with WHERE clause version #0 (randInt=0)
	; Disabled until issues 346,353 are resolved, also isn't yet finished
	if (randInt=10) do
	. new operator,leftSide,rightSide
	. set operator=$$comparisonOperators
	. set chosenColumn=$$chooseColumn("")
	. set leftSide=""
	. set rightSide=""
	. if ($random(2))  set leftSide=chosenColumn
	. else  set leftSide=$$chooseEntry(table,chosenColumn)
	. if ($random(2))  set rightSide=chosenColumn
	. else  set rightSide=$$chooseEntry(table,chosenColumn)
	. set result=result_"(("_leftSide_" "_operator_" "_rightSide_") = "_$$tf_")"

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#group%20by%20clause
groupbyClause()
	new result,randInt,i
	set result=" GROUP BY "

	set randInt=$random(3)+1
	for i=1:1:randInt do
	. set commaString=" "
	. if (i<randInt) set commaString=", "
	. set result=result_$$chooseColumn("")_commaString

	quit result

; #FUTURE_TODO: Expand this with more "aggregrate functions" besides COUNT as there has to
;               be more valid keywords/functions, do this when the HAVING clause is in Octo
; https://ronsavage.github.io/SQL/sql-92.bnf.html#having%20clause
havingClause()
	new result,randInt,max,randFromMax
	set result=" HAVING "

	set max=$$maxIndex(table)
	set randFromMax=$random(max-1)+1

	set result=result_"COUNT(("_$$chooseColumn("")_") "_$$comparisonOperators_" "_($random(max-1)+1)_")"
	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#order%20by%20clause
orderbyClause()
	new result,randInt,i,holder
	set result=" ORDER BY "

	set holder=" "
	for  quit:(holder="")  do
	. set holder=$order(selectListLVN(holder))
	. if (holder'="*")  set result=result_holder_", "

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

; Cannot find a single link that defines all of the Join clause, just the smaller portions of it
; #FUTURE_TODO: Have the potential to do more JOINS, as much as a 5-way JOIN. Do this using aliases.
;               This is currently disabled down in generateQuery() with issue 443 and issues 444,445
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
	. for i=1:1:15  quit:(($piece(type2,"(")=$piece(type1,"("))!(limiter=3))  do
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
	. . if $increment(joinNum)
	. . set result=result_chosenTable2_" AS join"_joinNum
	. . set chosenTable2="join"_joinNum
	. if (limiter=3)  set result=""

	if (randInt=1)  do
	. new subquery,i
	. set subquery=$$generateSubQuery("full")
	. if $increment(aliasNum,-1)
	. for i=0:1:$random(innerQuerySLLVN)  do
	. . set chosenColumn2=$order(innerQuerySLLVN("alias"_aliasNum,""))
	. set chosenTable2=innerQuerySLLVN("alias"_aliasNum,chosenColumn2)
	. set type2=$$returnColumnType(chosenTable2,chosenColumn2)
	.
	. for i=1:1:15  quit:($piece(type2,"(")=$piece(type1,"("))  do
	. . set chosenColumn1=$$chooseColumnBasedOffTable(chosenTable1)
	. . set type1=$$returnColumnType(chosenTable1,chosenColumn1)
	.	set chosenEntry1=$$chooseEntry(chosenTable1,chosenColumn1)
	.
	. set chosenTable2="alias"_aliasNum
	. if (i'=15)  set result=result_"("_subquery_") AS alias"_aliasNum
	. if (i=15)  set result=""

	; #FUTURE_TODO: Turn this into a recursive call, or change it such that the left side
	;               and right sides are reassigned for each iteration of the loop/recursion
	new i
	set onClause=""
	set loopCount=$random(3)+1
	set opened="FALSE"
	if ((result'="")&(joinType'="CROSS")&(joinType'="NATURAL"))  do
	. for i=1:1:loopCount  do
	. . set notString=""
	. . if $random(2) set notString=" NOT"
	. . if ($random(2))  set leftSide=chosenTable1_"."_chosenColumn1
	. . else  set leftSide=chosenEntry1
	. . if (($random(2))!(chosenEntry2=""))  set rightSide=chosenTable2_"."_chosenColumn2
	. . else  set rightSide=chosenEntry2
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
	. set onClause=" ON ("_$extract(onClause,1,$length(onClause))_")"
	. if ((i=loopCount)&(opened="TRUE"))  set onClause=onClause_")"

	quit result_onClause

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
	quit tables($random(tables))

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

columnCounter(currentTable)
	set column=""
	set columnCount=1
	set x=""
	for  set x=$order(sqlInfo(currentTable,x))  quit:x=""  if $increment(columnCount)
	quit columnCount

chooseColumnBasedOffTable(table)
	set CC=$SELECT($data(tableColumnCounts(table))=0:$$columnCounter(table),1:tableColumnCounts(table))
	set selectedColumn=$order(columns(table,$random(CC-1)+1,$random(CC-1)+1))
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

returnColumnIndex(tableName,columnName)
	; Returns index of column
	set index=0
	for  quit:($order(columns(tableName,index,""))=columnName)  if $increment(index)
	quit index

returnColumnType(tableName,columnName)
	set index=$$returnColumnIndex(tableName,columnName)
	quit $order(sqlInfo(tableName,index,columnName,""))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
generateQuery()
	new fc,i
	set query="SELECT "
	set query=query_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	set query=query_$$selectList($random(GLOBALtotalTables)+1)
	set query=query_" "_fc
	; #FUTURE_TODO: Uncomment lines pertaining to setting joinCount when Issues 444,445 are resolved
	if (($random(2))&(GLOBALtotalTables>1))  do
	. ;if ($random(2**5)=0)  set joinCount=5
	. ;if ($random(2**4)=0)  set joinCount=4
	. ;if ($random(2**3)=0)  set joinCount=3
	. ;if ($random(2**2)=0)  set joinCount=2
	. ;else  set joinCount=1
	. set joinCount=1
	.
	. ; Multiway JOINs are only currently supported with INNER JOIN,
	. ; following check enforces this behavior, remove check when Issue #311 is resolved
	. if (joinCount=1)  set outerJoinsAllowed="TRUE"
	.
	. for i=1:1:joinCount  set query=query_$$joinClause  if $increment(aliasNum)
	set query=query_$$tableExpression
	quit query_";"

; Valid parameters are "full" and "limited"
; Passing "full" returns a very general query with minimal restrictions
; Passing "limited" returns a query that contains clauses that limit the
; query to only return a single row, and single column
generateSubQuery(subQueryType)
	new fc,alias
	set alias="alias"_aliasNum

	merge tableColumnTemp=tableColumn
	merge selectListLVNTemp=selectListLVN
	new tableColumn,selectListLVN

	new innerQuery
	set innerQuery="SELECT "
	set innerQuery=innerQuery_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	if (subQueryType="full")  set innerQuery=innerQuery_$$innerSelectList(subQueryType,$random(3)+1,alias)
	if (subQueryType="limited")  set innerQuery=innerQuery_$$innerSelectList(subQueryType,0,alias)
	set innerQuery=innerQuery_" "_fc_" "_alias
	set innerQuery=innerQuery_$$innerTableExpression(subQueryType)

	new tableColumn,selectListLVN
	merge tableColumn=tableColumnTemp
	merge selectListLVN=selectListLVNTemp
	if $increment(aliasNum)
	quit innerQuery

; #FUTURE_TODO: Try to make as many of these the same as their non-inner specific ounterparts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Inner Specific Versions of Other Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
innerSelectList(subQueryType,curDepth,alias)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""

	; Qualifier notation (table.column), with the value of the alias variable instead of a table
	set toBeAdded=alias_"."_$$chooseColumn("")

	if (subQueryType="full")  do
	. for i=1:1:15  quit:($data(selectListLVN(toBeAdded))=0)  do
	. . set toBeAdded=alias_"."_$$chooseColumn("")
	.
	. if (i'=15)  do
	. . set selectListLVN(toBeAdded)=""
	. . set innerQuerySLLVN($piece(toBeAdded,"."),$piece(toBeAdded,".",2))=$piece(fc," ",2)
	. . if $increment(innerQuerySLLVN)
	. . set result=result_toBeAdded
	.
	. if (curDepth>0) do
	. . if $increment(curDepth,-1) ; to drop down a "level" in depth
	. . if (curDepth'=0) set result=result_", "_$$innerSelectList(subQueryType,curDepth,alias)
	.
	. if (i=15)  set curDepth=0  set result=$extract(result,0,$length(result)-1)

	if (subQueryType="limited")  do
	. set selectListLVN(toBeAdded)=""
	. set result=result_toBeAdded

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
innerTableExpression(subQueryType)
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen
	set innerResult=""

	; #FUTURE_TODO: Avoid WHERE clause selection if OUTER JOINs are used in the query (related to Issue 311).
	;               When 311 is resolved, the line containing `$find` calls below (to search for OUTER JOIN usages)
	;               can be removed and the line after it can instead be uncommented.
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	if ($random(2))  set innerResult=innerResult_$$innerWhereClause
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	if (('$random(8))&(subQueryType="full"))  set innerResult=innerResult_$$orderbyClause_$$limitClause
	if (subQueryType="limited")  set innerResult=innerResult_$$orderbyClause_" LIMIT 1"

	quit innerResult

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
innerWhereClause()
	new result,randInt,i,x
	set result=" WHERE "
	set randInt=$random(9) ; 0-8 ; Increase this range as new versions of WHERE clauses are added

	; #FUTURE_TODO: Fix WHERE clause version 5 (randInt=5) of innerWhereClause and remove following
	;               check (currently disabled).
	; randInt=5 does not work when there is also a JOIN with subquery in the query.
	; This occurs as a result of the desired alias(#) not existing in innerQuerySLLVN
	; randInt=5 also does not work as a result of it creating the "infinite" EXISTS loop
	; EX: ... WHERE EXISTS subquery(... WHERE EXISTS subquery(... WHERE EXISTS subquery(... WHERE EXISTS)))
	for  quit:(randInt'=5)  set randInt=$random(9)

	set table=$piece(fc," ",2)

	set type=""

	; When randInt=2 or 8 a VARCHAR type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 2 is string concatenation, which requires at least one VARCHAR in order to function
	; WHERE clause type 8 is LIKE with wildcards, this comparison can only occur on VARCHARs, not numeric/integer
	if ((randInt=2)!(randInt=8))  do
	. set x="sqlInfo("""_table_""")"
	. for i=1:1  do  quit:($find(type,"VARCHAR")'=0)
	. . set x=$query(@x)
	. . set type=$qsubscript(x,4)
	. . if ($qsubscript(x,1)'=table)  set randInt=0

	; When randInt=9 a BOOLEAN type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 9 is just "WHERE boolean-type-column"
	; Currently commented out until issue 346 is resolved
	;if (randInt=9)  do
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
	. . set chosenColumn=$$chooseColumn("")
	. . set leftSide=""
	. . set rightSide=""
	. . if $random(2)  set leftSide="alias"_aliasNum_"."_chosenColumn
	. . else  set leftSide=$$chooseEntry(table,chosenColumn)
	. . if $random(2)  set rightSide="alias"_aliasNum_"."_chosenColumn
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
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set result=result_"(("_"alias"_aliasNum_"."_chosenColumn_plusMinus_$$chooseEntry(table,chosenColumn)_")="_$$chooseEntry(table,chosenColumn)_")"

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
	. . set chosenColumn1=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn1)
	. set entry1=$$chooseEntry(table,chosenColumn1)
	. set entry1=$extract(entry1,2,$length(entry1)-1)
	. set leftSide="alias"_aliasNum_"."_chosenColumn1
	. if $random(2)  set leftSide=$$chooseEntry(table,chosenColumn1)
	.
	. set chosenColumn2=$$chooseColumn("")
	. set entry2=$$chooseEntry(table,chosenColumn2)
	. set entry2=$extract(entry2,2,$length(entry2)-1)
	. set rightSide="alias"_aliasNum_"."_chosenColumn2
	. if $random(2)  set rightSide=$$chooseEntry(table,chosenColumn2)
	.
	. set result=result_"(("_leftSide_"||"_rightSide_")"_$$comparisonOperators_"'"_entry1_entry2_"')"

	if (randInt=3) do
	. new type,chosenColumn,plusMinus,plusMinus2
	. ; ... WHERE customer_id=-(-3)
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. set plusMinus="+"
	. if $random(2) set plusMinus="-"
	. set plusMinus2="+"
	. if $random(2) set plusMinus2="-"
	. set result=result_"("_"alias"_aliasNum_"."_chosenColumn_" "_$$comparisonOperators_" "_plusMinus_"("_plusMinus2_$$chooseEntry(table,chosenColumn)_"))"

	if (randInt=4) do
	. new type,chosenColumn,beginning,plusMinus,end,aOperator
	. ; ... WHERE customer_id=-(-3)
	. ; ... WHERE (math expression) arithmetic operator -(math expression)
	.
	. set type=""
	. for  quit:($find(type,"INTEGER")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
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
	. set result=result_"(("_beginning_" "_aOperator_" "_end_") "_$$comparisonOperators_" "_"alias"_aliasNum_"."_chosenColumn_")"

	if (randInt=5) do
	. new notString,alias
	. ; ... WHERE EXISTS (SELECT ... query)
	. set notString=""
	. if $random(2) set notString="NOT "
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("full")_")"

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$chooseColumn("")
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. set limit=($random($$maxIndex(table)-1)+1)
	. if (limit>8)  set limit=8
	. for i=1:1:limit do
	. . set entryList=entryList_$$chooseEntry(table,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" "_notString_"IN ("_entryList_")"

	if (randInt=7) do
	. new chosenColumn
	. set chosenColumn=$$chooseColumn("")
	. ; ... WHERE column BETWEEN entry1 and entry2
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	if (randInt=8) do
	. new chosenColumn,string,i,randInt
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
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
	. set result=result_"alias"_aliasNum_"."_chosenColumn_" LIKE '"_string_"'"

	; #FUTURE_TODO: Add more complexity here (boolean operators mostly)
	;               WHERE NOT booleanColumn, WHERE booleanColumn=TRUE/FALSE
	; Currently disabled until issue 346 is resolved
	if (randInt=9) do
	. set type=""
	. for  quit:($find(type,"BOOLEAN")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. . ;if (type="BOOLEAN")  set result=result_chosenColumn
	. set result=result_chosenColumn

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	; #FUTURE_TODO: Maybe combine this with WHERE clause version #0 (randInt=0)
	; Disabled until issues 346 are resolved, also isn't yet finished
	if (randInt=10) do
	. new operator,leftSide,rightSide
	. set operator=$$comparisonOperators
	. set chosenColumn=$$chooseColumn("")
	. set leftSide=""
	. set rightSide=""
	. if ($random(2))  set leftSide=chosenColumn
	. else  set leftSide=$$chooseEntry(table,chosenColumn)
	. if ($random(2))  set rightSide=chosenColumn
	. else  set rightSide=$$chooseEntry(table,chosenColumn)
	. set result=result_"(("_leftSide_" "_operator_" "_rightSide_") = "_$$tf_")"

	quit result
