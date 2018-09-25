PACK(source,keys)
  SET res=source_"("
  SET i=0
  FOR  Q:$D(keys(i))=0  SET:i>0 res=res_"," SET res=res_"keys("_i_")" IF $I(i)
  SET res=res_")"
  QUIT res
