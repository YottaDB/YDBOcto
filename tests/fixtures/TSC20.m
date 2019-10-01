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

TSC20	;
	; Have test of select column list values that are long.
	; Test interesting values (see str2mval^%ydboctoplanhelpers for details)
	;
	new i,j,k,rand,str,file
	; Note: We stop at 1048565 as at higher values, we end up with MAXSTRLEN errors
	for i=125,126,127,128,16380,16381,16382,16383,32768,65536,131072,262144,524288,1048563,1048564,1048565 do
	.	for  set rand=1+$random(i)  quit:'$data(rand(rand))&(i'=rand)
	.	for j=i,rand do
	.	.	; Randomly create `value` column that is slightly different from `id`.
	.	.	; Take care not to go to 1048566 or else we would end up with MAXSTRLEN errors
	.	.	set k=j+$select('$random(8)&(1048565>j):1,1:0)
	.	.	set rand(j)=k
	.	.	set ^longvalues(j)=j_"|"_$justify(k,k)
	; Create reference file for later verification against Octo running SQL queries
	set file="TSC20.ref"
	open file:(newversion:stream:nowrap)
	use file:(width=65535:nowrap)		; needed to write arbitrarily long lines
	; Write expected output for : select id from longvalues order by id;
	set i="" for  set i=$order(rand(i)) quit:""=i  write i,!
	; Write expected output for : select id from longvalues where value ~ id::varchar order by id;
	set i="" for  set i=$order(rand(i)) quit:""=i  write:i=rand(i) i,!
	; Write expected output for : select value from longvalues order by id;
	set i="" for  set i=$order(rand(i)) quit:""=i  write $justify(rand(i),rand(i)),!
	; Write expected output for : select id,value from longvalues order by id;
	set i="" for  set i=$order(rand(i)) quit:""=i  write i_"|"_$justify(rand(i),rand(i)),!
	write !	; octo produces this line at the end (due maybe to its use of readline) so do the same here
	close file
	quit
