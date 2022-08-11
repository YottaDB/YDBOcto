;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The below M program generates random queries that use grouped/ungrouped outer query column values in inner queries
; This is a good test of YDBOcto#775/YDBOcto#806/YDBOcto#807/YDBOcto#819/YDBOcto#820/YDBOcto#870

test	;
	set maxdepth=4	; maximum sub-query depth
	for i=1:1:10 do
	. new where,having,groupby,orderby
	. write $$query(1),";"
	. write:$data(orderby) " -- sort-needed-check"
	. write !
	quit

query(depth)	;
	quit:depth>maxdepth ""
	new query
	set query="select "
	set query=query_$$selcollist(depth)
	set query=query_" from names n"_depth
	set query=query_$$where(depth)
	set query=query_$$groupby(depth)
	set query=query_$$having(depth)
	set query=query_$$orderby(depth)
	kill groupby(depth)
	quit query

selcollist(depth);
	new query
	set query=$$boolexpr(depth)
	quit query

where(depth)	;
	quit:'$random(2) ""
	new query
	set where(depth)=""
	set boolexpr=$$boolexpr(depth)
	kill where(depth)
	set query=" where "_boolexpr
	quit query

groupby(depth)	;
	quit:'$random(2) ""
	new query
	new lclDepth
	set lclDepth=$random(depth)+1
	if (1=$data(groupby(lclDepth))) do
	. set query=" group by n"_(lclDepth)_".firstname"
	else  do
	. ; Since $$groupby already skips execution 50% of the time avoid skipping it even more
	. ; by having this else block, it ensures that group by exists 50% of the time.
	. set query=" group by n"_depth_".firstname"
	. set groupby(depth)=""
	quit query

having(depth)	;
	quit:'$random(2) ""
	new query
	set having(depth)=""
	set boolexpr=$$boolexpr(depth)
	kill having(depth)
	set query=" having "_boolexpr
	quit query

orderby(depth);
	quit:'$random(2) ""
	set:1=depth orderby=""
	quit " order by "_$$boolexpr(depth)

boolexpr(depth)
	quit:$random(2)&(depth<maxdepth) "exists ("_$$query(depth+1)_")"
	new lhs,rhs,query
	set lhs=$$scalar(depth)
	set rhs=$$scalar(depth)
	set query=lhs_$$relop_rhs
	quit query

relop()	;
	new choice
	set choice=$random(4)
	quit:choice=1 " > "
	quit:choice=2 " < "
	quit:choice=3 " = "
	quit " != "

scalar(depth)
	quit:'$random(4) $random(10)
	new query
	if $random(2) set query=$$aggregate(depth) quit:""'=query query
	quit $$columnreference(depth)

columnreference(depth)
	set depth=$random(depth)+1
	quit:$data(where(depth)) "n"_depth_".id"
	quit "count(n"_depth_".id)"

aggregate(depth)
	new i,aggregate
	set aggregate=""
	for i=1:1:depth do
	. quit:'$random(2)
	. set:$data(groupby(i)) aggregate=aggregate_$select(""=aggregate:"",1:" || ")_"n"_i_".firstname"
	quit:""'=aggregate "count("_aggregate_")"
	quit ""

