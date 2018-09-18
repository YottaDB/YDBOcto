run(cursorId)
  SET t=""
  FOR  SET t=$O(^cursor(cursorId,"exe",t)) QUIT:(t="")  XECUTE ^cursor(cursorId,"exe",t)
  QUIT
