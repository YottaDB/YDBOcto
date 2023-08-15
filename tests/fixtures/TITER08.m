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
TITER08 ; ITERATOR key-fixing is per-column (VIRTUAL vs non-VIRTUAL)
catsys(n) quit $order(^catrectitles(n))
id(catsys,n)
 if n>2 q ""
 set n=n+1
 quit n
idx(catsys,id,n)
 if n>2 q ""
 set n=n+1
 kill RAti
 set RAti("lg")=catsys_id_"foo"
 set RAti("ty")=catsys_id_"boo"
 set RAti("so")=catsys_id_"coo"
 set RAti("ti")=catsys_id_"doo"
 quit n
