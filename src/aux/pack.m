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

PACK(source,keys)
  SET res=source_"(",i=0
  SET table=$O(keys(""))
  FOR  QUIT:table=""  DO  SET table=$O(keys(table))
  . SET column=$O(keys(table,""))
  . FOR  QUIT:column=""  DO
  . . SET:i>0 res=res_","
  . . SET res=res_"keys("""_table_""","""_column_""")"
  . . SET column=$O(keys(table,column))
  . . IF $I(i)
  SET res=res_")"
  QUIT res
