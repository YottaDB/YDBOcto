;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collection of helper M functions that are used by the `test-auto-upgrade` pipeline job

testAutoUpgrade
	QUIT

batsTestsChooseRandom
	;
	; Reads all lines of stdin and randomly filter out certain lines containins "ADD_BATS_TEST(..)"
	; This reduces the runtime of the pipeline test as only a fraction of the bats tests get run.
	; The randomness ensures multiple pipeline job runs cover all tests eventually.
	;
	NEW i,line,nLines,include,batsTestName
        FOR  READ line($increment(nLines)) QUIT:$zeof
	KILL line(nLines) if $increment(nLines,-1)
	; Make a list of tests that we never want to exclude
	SET include("hello_psql")=""
	SET include("test_query_generator")=""
	FOR i=1:1:nLines DO
	.	NEW skip
	.	SET skip=0
	.	IF (line(i)["ADD_BATS_TEST(") DO  QUIT:skip
	.	.	SET batsTestName=$PIECE($PIECE(line(i),"ADD_BATS_TEST(",2),")",1)
	.	.	QUIT:$DATA(include(batsTestName))  ; check if test cannot be excluded
	.	.	; We pick only 10% of the bats tests. This is because we have seen that picking 25% resulted in
	.	.	; the `test-auto-upgrade` pipeline job sometime running for as high as 45 minutes which is more
	.	.	; than the main jobs (`make-ubuntu` etc.). Hence reduced it to 10% since with enough number of
	.	.	; pipeline runs, we will see good coverage eventually and keeping each pipeline run reasonably short.
	.	.	SET skip=$RANDOM(10) ; Include 10% of the tests, Skip 90% of the tests
	.	WRITE line(i),!
	QUIT
