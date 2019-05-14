run(cursorId,filename)
  ;filename includes fullpath, only want last element
  SET filename=$PIECE($PIECE(filename,"/",$LENGTH(filename,"/")),".",1)
  ZLINK filename
  SET cmd="do ^"_$TRANSLATE(filename,"_","%")_"(cursorId)"
  XECUTE cmd
  USE $P
  QUIT
