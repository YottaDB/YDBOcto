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

	set initDone("setQuantifier")=0
	set initDone("chooseTable")=0
	set initDone("chooseColumn")=0
	set initDone("arithmeticOperator")=0
	set initDone("comparisonOperators")=0
	set initDone("joinTypes")=0
	set GLOBALtotalTables=0
	set orderByExists="FALSE"
	set limitExists="FALSE"

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
	. kill tableColumn,selectListLVN,subQuerySelectedTables,tableColumnCopy

	quit

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
	. . . if $increment(GLOBALtotalTables)
	. .
	. . ;set i2=1
	. . ;COLUMN NAMES
	. . if (($find(line(i),"INTEGER")'=0)!($find(line(i),"VARCHAR")'=0)) do
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

	; #FUTURE_TODO: Change this code block to not "quit "*"", but rather just "set result="*"" also Issue #385
	; Choose whether to use a wildcard or build a select list
	set randInt=$random(4) ;25% chance to get an asterisk, 75% chance to continue further
	if ((randInt=0)&(curDepth=0)) quit "*"

	set result=""
	; Choose DerivedColumn or Qualifier
	set randInt=$random(2)
	; To avoid ambiquity warnings, this is commented out
	; Regular column notation is to be used
	;if randInt=0 set toBeAdded=$$chooseColumn("")

	; Below check is to ensure that if there is only 1 table, that there can be no subquerying
	if (GLOBALtotalTables=1) set randInt=0

	; Qualifier notation (table.column) is to be used
	if (randInt=0)  do
	. set table=$order(tableColumn(1))
	. set toBeAdded=table_"."_$$chooseColumn(table)
	. set selectListLVN(toBeAdded)=""

	if (randInt=1)  do
	. set alias="alias"_aliasNum
	. set toBeAdded="("_$$generateSubQuery("selectlist")_") AS "_alias
	. set selectListLVN(alias)=""

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

	; #FUTURE_TODO: Remove following line, and reenable following commented out line
	;               when Issue 311 is resolved
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	;if (($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))&($find(query," AS ")=0))  set result=result_$$whereClause
	if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$whereClause
	;if (($random(2))  set result=result_$$whereClause
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	; #FUTURE_TODO: Remove following line, and reenable following commented out line
	;               when Issue 311 is resolved
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$orderbyClause
	;if $random(2)  set result=result_$$orderbyClause  set orderByExists="TRUE"
	if '$random(8)  set result=result_$$limitClause  set limitExists="TRUE"

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
	;set randInt=$random(2)+1 ; 1,2 #FUTURE_TODO: Make this a random value based off of total amount of tables
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

	set tableColumn(chosenTable)=""

	quit result_chosenTable ;chosenTable OR fromTableList

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
whereClause()
	new result,randInt,i
	set result=" WHERE "
	set randInt=$random(9) ; 0-8

	for i=1:1:2 set table=$piece(fc," ",i)

	; When randInt=2 or 8 a VARCHAR type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value.
	; WHERE clause type 2 is string concatenation, which requires at least one VARCHAR in order to function
	; WHERE clause type 8 is LIKE with wildcards, this comparison can only occur on VARCHARs, not numeric/integer
	set type=""
	if ((randInt=2)!(randInt=8))  do
	. set x="sqlInfo("""_table_""")"
	. for i=1:1  do  quit:($find(type,"VARCHAR")'=0)
	. . set x=$query(@x)
	. . set type=$qsubscript(x,4)
	. . if ($qsubscript(x,1)'=table)  set randInt=0

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	if (randInt=0) do
	. new loopCount,i,leftSide,rightSide,notString,andOrChoice,chosenColumn
	. set loopCount=$random(3)+1 ; 1-3
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
	. ;set alias="q2"
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("exists")_")" ;" "_alias_")"

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

	; #FUTURE_TODO: Make this pretty/better and improve on the string, maybe focus more on wildcards
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
	. . set randInt=$random(7) ; 0,1,2,3,4,5,6
	. . if (randInt=0) set string=string_$char($random(27)+64) ; @,A-Z
	. . if (randInt=1) set string=string_$char($random(26)+97) ; a-z
	. . if (randInt=2) set string=string_$random(10) ; 0-9
	. . if (randInt=3) set string=string_"#"
	. . if (randInt=4) set string=string_"$"
	. . if (randInt=5) set string=string_"%"
	. . if (randInt=6) set string=string_"_"
	. set result=result_table_"."_chosenColumn_" LIKE '"_string_"'"

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

; Cannot find a single link that defines all of the Join clause, just the smaller portions of it
; #FUTURE_TODO: Have the potential to do more JOINS, as much as a 5-way JOIN. Do this using aliases.
;               For the ON clause each table has to only be joined once, no duplicates as the joins get "larger"
; #FUTURE_TODO: Implement sub-queries into JOINs, with aliases.
; #FUTURE_TODO: Add AND, OR, and NOT into the ON clause condtions
joinClause()
	new result,chosenTable1,chosenColumn1,type1,chosenTable2,chosenColumn2,type2,joinType,operator,fullString,i
	set result=""

	set chosenTable1=$order(tableColumn(1)) ; essentially table1
	set chosenColumn1=$$chooseColumnBasedOffTable(chosenTable1)
	set type1=$$returnColumnType(chosenTable1,chosenColumn1)

	set chosenTable2=chosenTable1
	for  quit:(chosenTable2'=chosenTable1)  do
	. set chosenTable2=$$chooseTable ; essentially table2

	set chosenColumn2=""
	set type2=""

	; #FUTURE_TODO: Take another look at the following loop to see if it can be improved, which
	;               it likely should be able to but not in any obvious way currently
	;               At the minimum, add incompatible tables to a LVN, to make sure that they
	;               don't get randomly selected on later iterations. This LVN will also need to
	;               be killed alongside the others in the main loop.
	; After 15 attempts at finding two compatible columns (same type), select a new chosenTable2
	; repeat this 2 more times and if this fails to produce a column that is valid, then delete
	; the join portion of the query
	set limiter=1
	for i=1:1:15  quit:(($piece(type2,"(")=$piece(type1,"("))!(limiter=3))  do
	. if (i=15)  do
	. . set i=1
	. . if $increment(limiter)
	. . set chosenTable2=chosenTable1
	. . for  quit:(chosenTable2'=chosenTable1)  do
	. . . set chosenTable2=$$chooseTable ; essentially table2
	. set chosenColumn2=$$chooseColumnBasedOffTable(chosenTable2)
	. set type2=$$returnColumnType(chosenTable2,chosenColumn2)

	set joinType=$$joinTypes
	set operator=$$comparisonOperators

	; Must always compare with an "=" as PostgreSQL issues the following error when FULL join:
	; FULL JOIN is only supported with merge-joinable or hash-joinable join conditions
	if (joinType="FULL")  set operator="="

	; #FUTURE_TODO: If an OUTER JOIN is selected, the only valid/working operator is "=", remove this force setting when Issue 311 is resolved
	;               Also see tests/fixtures/TOJ03.m line 103 and 114 for more information pertaining to this problem
	if ((joinType="LEFT")!(joinType="RIGHT")!(joinType="LEFT OUTER")!(joinType="RIGHT OUTER")) set operator="="

	; Randomly tests FULL OUTER JOIN, as compared to just OUTER JOIN
	set fullString=" "
	if (($random(2))&(joinType="OUTER"))  set fullString=" FULL "  set operator="="

	; #FUTURE_TODO: also have literal in either side or both sides of the ON clause
	; SELECT column(s) FROM table1 LEFT/RIGHT/INNER/OUTER JOIN table2 ON table1.column comparisonOperator = table2.column
	set result=fullString_joinType_" JOIN "_chosenTable2_" ON "_"("_chosenTable1_"."_chosenColumn1_" "_operator_" "_chosenTable2_"."_chosenColumn2_")"

	if (limiter=3)  set result=""

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
	if (table="") set table=$order(tableColumn(""))
	new CC,selectedColumn

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

joinTypes()
	if (initDone("joinTypes")=0) do
	. set initDone("joinTypes")=1
	. set joinTypes=-1
	. set joinTypes($increment(joinTypes))="LEFT"
	. set joinTypes($increment(joinTypes))="LEFT OUTER"
	. set joinTypes($increment(joinTypes))="RIGHT"
	. set joinTypes($increment(joinTypes))="RIGHT OUTER"
	. set joinTypes($increment(joinTypes))="INNER"
	. if $increment(joinTypes)
	; #FUTURE_TODO: Remove following 3 lines, and uncomment 4th line when Issue 311 is resolved
	set joinType=joinTypes($random(joinTypes))
	if ($find(query,"DISTINCT")'=0) set joinType="INNER"
	quit joinType
	;quit joinTypes($random(joinTypes))

; #FUTURE_TODO: Try to combine generateExistsSubQuery and generateSelectListSubQuery as
;               they are identical besides a single parameter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
generateQuery()
	new fc
	set query="SELECT "
	set query=query_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	set query=query_$$selectList($random(GLOBALtotalTables)+1)
	set query=query_" "_fc
	if (($random(2))&(GLOBALtotalTables>1))  set query=query_$$joinClause
	set query=query_$$tableExpression
	quit query_";"

generateSubQuery(subQueryType)
	new fc,alias
	set alias="alias"_aliasNum

	; #FUTURE_TODO: Resolve the following issue/lack of feature
	; This function is missing the call to tableExpression as that has
	; the potential to cause an infinite loop with a bunch of EXISTS
	; statements, probably rare but possible.
	; It also just breaks things apparently.

	merge tableColumnTemp=tableColumn
	merge selectListLVNTemp=selectListLVN
	new tableColumn,selectListLVN

	new innerQuery
	set innerQuery="SELECT "
	set innerQuery=innerQuery_$$setQuantifier
	set fc=$$fromClause ; fromClause needs to run before selectList
	if (subQueryType="exists")  set innerQuery=innerQuery_$$existsSubQuerySelectList($random(3)+1,alias)
	if (subQueryType="selectlist")  set innerQuery=innerQuery_$$slSubQuerySelectList(0,alias)
	set innerQuery=innerQuery_" "_fc_" "_alias
	if (subQueryType="exists")  set innerQuery=innerQuery_$$innerTableExpression
	if (subQueryType="selectlist")  set innerQuery=innerQuery_$$slInnerTableExpression

	new tableColumn,selectListLVN
	merge tableColumn=tableColumnTemp
	merge selectListLVN=selectListLVNTemp
	if $increment(aliasNum)
	quit innerQuery

; #FUTURE_TODO: Try to make as many of these the same as their non-inner specific ounterparts
; #FUTURE_TODO: Also try to squash as many of these together as possible, maybe through
;               use of parameters to force or skip certain code paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Inner Specific Versions of Other Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
existsSubQuerySelectList(curDepth,alias)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""

	; Qualifier notation (table.column), with the value of the alias variable instead of a table
	set toBeAdded=alias_"."_$$chooseColumn("")

	set selectListLVN(toBeAdded)=""
	set result=result_toBeAdded

	if curDepth>0 do
	. if $increment(curDepth,-1) ; to drop down a "level" in depth
	. if (curDepth'=0) set result=result_", "_$$existsSubQuerySelectList(curDepth,alias)

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#select%20list
slSubQuerySelectList(curDepth,alias)
	new randInt,result,toBeAdded
	;This function serves the same purpose as select sublist in the grammar rules for SQL.

	set result=""

	; Qualifier notation (table.column), with the value of the alias variable instead of a table
	set toBeAdded=alias_"."_$$chooseColumn("")

	set selectListLVN(toBeAdded)=""
	set result=result_toBeAdded

	quit result

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
innerTableExpression()
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen
	set innerResult=""

	; #FUTURE_TODO: Remove following line, and reenable following commented out line
	;               when Issue 311 is resolved
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	; #FUTURE_TODO: Fix and reenable the where clause, infinite loop esque crash
	;if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$innerWhereClause
	;if ($random(2))  set result=result_$$innerWhereClause
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	if $random(2)  set innerResult=innerResult_$$orderbyClause
	if '$random(8)  set innerResult=innerResult_$$limitClause

	quit innerResult

; https://ronsavage.github.io/SQL/sql-92.bnf.html#table%20expression
slInnerTableExpression()
	; From Clause should go here, but it needs to be decided on early as to
	; allow for proper column(s) to be chosen
	set innerResult=""

	; #FUTURE_TODO: Remove following line, and reenable following commented out line
	;               when Issue 311 is resolved
	; The following line assumes that LEFT, LEFT OUTER, RIGHT, RIGHT OUTER can only exist in the
	; query string as the join operators, added spaces before and after them to try to limit the
	; overall effect of this
	; #FUTURE_TODO: Fix and reenable the where clause, infinite loop esque crash
	;if ($random(2))&'(($find(query," LEFT OUTER ")'=0)!($find(query," RIGHT ")'=0)!($find(query," RIGHT OUTER ")'=0)!($find(query," LEFT ")'=0))  set result=result_$$innerWhereClause
	;if ($random(2))  set result=result_$$innerWhereClause
	; #FUTURE_TODO: Uncomment following line when GROUP BY is done in Octo Issue #55
	;if $random(2)  set result=result_$$groupbyClause
	; #FUTURE_TODO: Uncomment following line when HAVING is done in Octo Issue #55
	;if $random(2)  set result=result_$$havingClause
	; Following two lines lack condtions as they need to be present, as sub queries in the
	; select list must only return 1 row and 1 column
	set innerResult=innerResult_$$orderbyClause
	set innerResult=innerResult_" LIMIT 1"

	quit innerResult

; https://ronsavage.github.io/SQL/sql-92.bnf.html#from%20clause
innerFromClause()
	new result,randInt,i,x,chosenTable
	;Constructs a FROM clause from a random number of table references.

	set result="FROM "

	; #FUTURE_TODO: This limit number will need to worked on, right now this is just relying on
	;               lucky randomization and probability to make sure everything runs as expected
	set limit=100
	for i=1:1:limit  do  quit:($data(tableColumnCopy(chosenTable))=0)
	. set chosenTable=$$chooseTable
	. if (i=limit)  set chosenTable=""

	if (i'=limit)  set tableColumn(chosenTable)=""  set tableColumnCopy=(chosenTable)=""

	quit result_chosenTable ;chosenTable OR fromTableList

; https://ronsavage.github.io/SQL/sql-92.bnf.html#where%20clause
innerWhereClause()
	new result,randInt
	set result=" WHERE "
	set randInt=$random(9) ; 0-8
	set randInt=4
	;0,1,2,3,4,5

	; When randInt=2 or 8 a VARCHAR type column is necessary in the selected table,
	; this code block ensures that this requirement is satisfied, and if it isn't
	; then set randInt to a different value
	set type=""
	if ((randInt=2)!(randInt=8))  do
	. set x="sqlInfo("""_table_""")"
	. for i=1:1  do  quit:($find(type,"VARCHAR")'=0)
	. . set x=$query(@x)
	. . set type=$qsubscript(x,4)
	. . if ($qsubscript(x,1)'=table)  set randInt=0

	; #FUTURE_TODO: Boolean expressions can be combined to create more complex Booleans
	;               Example: ((id = 1) OR (firstname = 'Zero')) AND (lastname '= 'Cool')
	if (randInt=0) do
	. new loopCount,i,leftSide,rightSide,notString,andOrChoice,chosenColumn
	. set loopCount=$random(3)+1 ; value between inclusive 1 and 3
	. set i=1
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
	. new type,chosenColumn,beginning,plusMinus,end
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
	. set end=plusMinus_"("_plusMinus_$$chooseEntry(table,chosenColumn)_")"
	.
	. set result=result_"(("_beginning_" "_$$arithmeticOperator_" "_end_") "_$$comparisonOperators_" "_table_"."_chosenColumn_")"

	if (randInt=5) do
	. new notString,alias
	. ; ... WHERE EXISTS (SELECT ... query)
	. set notString=""
	. if $random(2) set notString="NOT "
	. ;set alias="q2"
	. set result=result_notString_"EXISTS ("_$$generateSubQuery("exists")_")" ;" "_alias_")"

	if (randInt=6) do
	. new chosenColumn,notString,entryList,i
	. set chosenColumn=$$chooseColumn("")
	. set notString=""
	. if $random(2) set notString="NOT "
	. set entryList=""
	. for i=1:1:($random($$maxIndex(table)-1)+1) do
	. . set entryList=entryList_$$chooseEntry(table,chosenColumn)_", "
	. ; strip the ", " off of entryList
	. set entryList=$extract(entryList,0,$length(entryList)-2)
	. set result=result_table_"."_chosenColumn_" "_notString_"IN ("_entryList_")"

	if (randInt=7) do
	. new chosenColumn
	. set chosenColumn=$$chooseColumn("")
	. ; ... WHERE column BETWEEN entry1 and entry2
	. set result=result_table_"."_chosenColumn_" BETWEEN "_$$chooseEntry(table,chosenColumn)_" AND "_$$chooseEntry(table,chosenColumn)

	; #FUTURE_TODO: Make this pretty/better and improve on the string, maybe focus more on wildcards
	; #FUTURE_TODO: Uncomment when Issue 398 (LIKE with wildcard characters) is fixed
	if (randInt=8) do
	. new chosenColumn,string,i,randInt
	. ; Forces column to be of type "VARCHAR" as to do a string comparison
	. set type=""
	. for  quit:($find(type,"VARCHAR")'=0)  do
	. . set chosenColumn=$$chooseColumn("")
	. . set type=$$returnColumnType(table,chosenColumn)
	. set string=""
	. set desiredStringLength=$random(7)+1
	. for i=1:1:desiredStringLength do
	. . set randInt=$random(7) ; 0,1,2,3,4,5,6
	. . if (randInt=0) set string=string_$char($random(27)+64) ; @,A-Z
	. . if (randInt=1) set string=string_$char($random(26)+97) ; a-z
	. . if (randInt=2) set string=string_$random(10) ; 0-9
	. . if (randInt=3) set string=string_"#"
	. . if (randInt=4) set string=string_"$"
	. . if (randInt=5) set string=string_"%"
	. . if (randInt=6) set string=string_"_"
	. set result=result_table_"."_chosenColumn_" LIKE '"_string_"'"

	quit result
