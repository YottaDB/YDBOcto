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
TAF10 ; ITERATOR functions of TAF10: those of the TAF10i table (Part E) return the key after the given one, "" at the end
a(n)
 quit $order(^TAF10i(n))
b(a,n)
 quit $order(^TAF10i(a,n))
odd(n)
 ; ITERATOR of the TAF10x table of TAF10 (Part F): the odd ids of ^TAF10x after n, "" at the end. The even ids are
 ; nodes of the global that are not rows of the table.
 for  set n=$order(^TAF10x(n)) quit:(""=n)!(n#2)
 quit n
virtual(n)
 ; VIRTUAL ITERATOR of the TAF10xv table of TAF10 (Part F): the ids 1, 2 and 3, which no global holds
 quit $select(""=n:1,n<3:n+1,1:"")
