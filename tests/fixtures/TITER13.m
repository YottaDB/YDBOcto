;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TITER13 ; ITERATOR explicit argument list test
; Same format as TITER01, but each iterator takes a leading literal "label"
; argument supplied by the user via the ITERATOR's parenthesized form.
catsys(label,n)
 if n>2 q ""
 set n=n+1
 quit n
id(label,catsys,n)
 if n>2 q ""
 set n=n+1
 quit n
idx(label,catsys,id,n)
 if n>2 q ""
 set n=n+1
 kill RAti
 set RAti(n,"lg")=label_"_"_catsys_id_"foo"
 set RAti(n,"ty")=label_"_"_catsys_id_"boo"
 set RAti(n,"so")=label_"_"_catsys_id_"coo"
 set RAti(n,"ti")=label_"_"_catsys_id_"doo"
 quit n
