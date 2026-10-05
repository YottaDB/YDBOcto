mcoverage ; Line coverage of Octo's M routines, from YottaDB M-profiling dumps
 ;
 ; Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.
 ; All rights reserved.
 ;
 ; This source code contains the intellectual property
 ; of its copyright holder(s), and is made available
 ; under a license.  If you do not know the terms of
 ; the license, please stop and do not read further.
 ;
 ; In a build with ENABLE_COVERAGE, createdb() maps ^%ydbcov to a region of the test's own database and sets
 ; ydb_trace_gbl_name to ^%ydbcov($J). Every M process the test starts from then on (octo, rocto, yottadb) dumps
 ; its M-profiling data at exit under its own pid:
 ;
 ;   ^%ydbcov(pid,routine,label,offset)="count:usertime:systime:totaltime"
 ;
 ; where offset counts lines from the label, comment lines included. A source line is covered if any process
 ; executed it.
 ;
 ; SAVE^mcoverage runs from corecheck() at the end of each test. It sums the counts over the test's processes into
 ; a file outside the test directory, as corecheck() deletes the test directory right after if the test passed.
 ; JSON^mcoverage runs from tools/ci/build.sh after ctest. It sums the files of every test and writes a gcovr JSON
 ; tracefile, which gcovr merges with the C coverage into the one Cobertura report.
 QUIT
 ;
SAVE(out) ; Write to the file out the line counts of Octo's routines, summed over this test's processes
 NEW count,label,offset,pid,routine
 SET pid="" FOR  SET pid=$ORDER(^%ydbcov(pid)) QUIT:""=pid  DO
 . SET routine="%ydbocto" FOR  SET routine=$ORDER(^%ydbcov(pid,routine)) QUIT:"%ydbocto"'=$EXTRACT(routine,1,8)  DO
 . . ; Physical plans (%ydboctoP*) and cross-reference plans (%ydboctoX*) are generated, not part of Octo's source
 . . QUIT:"PX"[$EXTRACT(routine,9)
 . . SET label="" FOR  SET label=$ORDER(^%ydbcov(pid,routine,label)) QUIT:""=label  DO
 . . . SET offset="" FOR  SET offset=$ORDER(^%ydbcov(pid,routine,label,offset)) QUIT:""=offset  DO
 . . . . QUIT:offset'=+offset
 . . . . IF $INCREMENT(count(routine,label,offset),+^%ydbcov(pid,routine,label,offset))
 ; No Octo routine ran in this test (e.g. it only ran shell commands), so there is nothing to record
 QUIT:'$DATA(count)
 OPEN out:(NEWVERSION) USE out
 SET routine="" FOR  SET routine=$ORDER(count(routine)) QUIT:""=routine  DO
 . SET label="" FOR  SET label=$ORDER(count(routine,label)) QUIT:""=label  DO
 . . SET offset="" FOR  SET offset=$ORDER(count(routine,label,offset)) QUIT:""=offset  DO
 . . . WRITE routine," ",label," ",offset," ",count(routine,label,offset),!
 CLOSE out
 QUIT
 ;
JSON(dir,root,out,version) ; Write the gcovr JSON tracefile out for every routine in root/src/aux
 ; dir:     directory holding the *.mcov files SAVE^mcoverage wrote, one per test
 ; root:    the repository root, which gcovr's --root names too; file names in the tracefile are relative to it
 ; out:     path of the JSON tracefile to write
 ; version: gcovr's JSON format version. gcovr reads only its own, so the caller takes it from a tracefile that
 ;          same gcovr wrote.
 NEW code,count,covered,file,files,hits,label,line,lineno,name,nexec,offset,rest,routine,source
 ; 1. Hits per (routine,label,offset), summed over every test
 FOR  SET file=$ZSEARCH(dir_"/*.mcov") QUIT:""=file  DO
 . OPEN file:(READONLY) USE file
 . FOR  READ line QUIT:$ZEOF  DO
 . . SET routine=$PIECE(line," ",1),label=$PIECE(line," ",2),offset=$PIECE(line," ",3),count=$PIECE(line," ",4)
 . . IF $INCREMENT(hits(routine,label,offset),count)
 . CLOSE file
 ; 2. Read each source and give each executable line its (label,offset)
 SET (covered,nexec)=0
 FOR  SET source=$ZSEARCH(root_"/src/aux/_ydbocto*.m") QUIT:""=source  DO
 . SET name=$ZPARSE(source,"NAME"),routine="%"_$ZEXTRACT(name,2,$ZLENGTH(name)),file="src/aux/"_name_".m"
 . SET files(file)="",label="",offset=0,lineno=0
 . OPEN source:(READONLY) USE source
 . FOR  READ line QUIT:$ZEOF  DO
 . . SET lineno=lineno+1
 . . IF line?1(1"%",1A).E DO
 . . . ; A label: its name runs to the first character that is not alphanumeric, and it restarts the offsets
 . . . SET label=$ZEXTRACT(line,1,$$namelen(line)),offset=0,rest=$ZEXTRACT(line,$ZLENGTH(label)+1,$ZLENGTH(line))
 . . . ; A formal list executes as a line of its own; otherwise the code after the label
 . . . SET code=$SELECT("("=$ZEXTRACT(rest):"(",1:$$trim(rest))
 . . ELSE  SET offset=offset+1,code=$$trim(line)
 . . QUIT:'$ZLENGTH(label)  QUIT:'$ZLENGTH(code)  QUIT:";"=$ZEXTRACT(code)
 . . SET files(file,lineno)=$GET(hits(routine,label,offset),0),nexec=nexec+1
 . . SET:files(file,lineno) covered=covered+1
 . CLOSE source
 ; 3. The tracefile
 OPEN out:(NEWVERSION) USE out
 WRITE "{""gcovr/format_version"": """,version,""", ""files"": ["
 SET file="" FOR  SET file=$ORDER(files(file)) QUIT:""=file  DO
 . WRITE $SELECT(""=$ORDER(files(file),-1):"",1:","),!,"{""file"": """,file,""", ""functions"": [], ""lines"": ["
 . SET lineno="" FOR  SET lineno=$ORDER(files(file,lineno)) QUIT:""=lineno  DO
 . . WRITE $SELECT(""=$ORDER(files(file,lineno),-1):"",1:","),!
 . . WRITE "{""line_number"": ",lineno,", ""count"": ",files(file,lineno),", ""branches"": []}"
 . WRITE "]}"
 WRITE !,"]}",!
 CLOSE out
 WRITE "M coverage: ",covered," of ",nexec," executable lines of Octo's M routines",!
 QUIT
 ;
namelen(line) ; Length of the label name at the start of line: letters, digits and a leading %
 NEW i,n
 SET n=1
 FOR i=2:1:$ZLENGTH(line) QUIT:$ZEXTRACT(line,i)'?1AN  SET n=i
 QUIT n
 ;
trim(s) ; s without leading spaces, tabs and the dots of a DO block ("" if that is all there is)
 NEW c,i
 ; The extra iteration past the end reads "" and stops there, so a line of nothing but whitespace trims to ""
 FOR i=1:1:$ZLENGTH(s)+1 SET c=$ZEXTRACT(s,i) QUIT:(" "'=c)&($CHAR(9)'=c)&("."'=c)
 QUIT $ZEXTRACT(s,i,$ZLENGTH(s))
