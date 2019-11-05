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
;
; This program reads an input query file (e.g. input.sql) with lots of queries and generates lots of query files
;	(input_01.sql, input_02.sql etc.) each containing one query.
; Expects each query to be contained in one line in the input query file (i.e. multi-line queries not supported yet).
; Also expects the input query file to be passed in the command line as the first/only parameter ($zcmdline).
;
genqueryfiles
	new queryfile,file,line,nlines,i,prefix,cnt
	set queryfile=$zcmdline
	open queryfile:(readonly)
	use queryfile
        for  read line($incr(nlines))  quit:$zeof
	close queryfile
	kill line(nlines)  if $incr(nlines,-1)
	set prefix=$piece(queryfile,".",1)
	for i=1:1:nlines do
	. set firstpiece=$piece(line(i)," ",1)
	. quit:(""=line(i))!("#"=$zextract(line(i),1))!("--"=$zextract(line(i),1,2))
	. set file=prefix_"_"_$translate($justify($incr(cnt),2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write line(i),!
	. close file
	quit
	;
