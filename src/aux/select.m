run(cursorId,filename,routine)
  ;filename includes fullpath, only want last element
  ZLINK filename
  SET cmd="do ^"_routine_"(cursorId)"
  XECUTE cmd
  USE $P
  QUIT
