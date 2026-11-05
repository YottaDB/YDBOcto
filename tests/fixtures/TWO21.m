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
TWO21 ; ITERATOR functions of the itr, itrs and itr3 tables of TWO21 (Part H): each returns the key after the given one, "" at the end (aodd skips the even values of a)
a(n)
 quit $order(^ITR(n))
b(a,n)
 quit $order(^ITR(a,n))
aodd(n)
 ; ITERATOR of a in the itrs table: the odd values of a after n, "" at the end. ^ITR(2,...) is not a row of itrs.
 for  set n=$order(^ITR(n)) quit:(""=n)!(n#2)
 quit n
a3(n)
 quit $order(^ITR3(n))
b3(a,n)
 quit $order(^ITR3(a,n))
c3(a,b,n)
 quit $order(^ITR3(a,b,n))
