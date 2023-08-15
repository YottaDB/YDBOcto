;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TITER01 ; ITERATOR basic test
catsys(n)
 if n>2 q ""
 set n=n+1
 quit n
id(catsys,n)
 if n>2 q ""
 set n=n+1
 quit n
idx(catsys,id,n)
 if n>2 q ""
 set n=n+1
 kill RAti
 set RAti(n,"lg")=catsys_id_"foo"
 set RAti(n,"ty")=catsys_id_"boo"
 set RAti(n,"so")=catsys_id_"coo"
 set RAti(n,"ti")=catsys_id_"doo"
 quit n
