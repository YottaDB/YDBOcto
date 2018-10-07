PACK(source,keys)
  write "here",!
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
