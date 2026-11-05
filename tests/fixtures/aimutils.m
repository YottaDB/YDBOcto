aimutils ; Utilites for testing YDBAIM in Octo
 ;
 ; Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.
 ; All rights reserved.
 ;
 ; This source code contains the intellectual property
 ; of its copyright holder(s), and is made available
 ; under a license.  If you do not know the terms of
 ; the license, please stop and do not read further.
 ;
killAIMData ; Remove ^%ydbAIMD globals
 SET (%,%1)="^%ydbAIMD"
 FOR  SET %=$ORDER(@%) QUIT:%'[%1  KILL @%
 QUIT
 ;
zwriteAIMData ; ZWRITE ^%ydbAIMD globals
 SET (%,%1)="^%ydbAIMD"
 FOR  SET %=$ORDER(@%) QUIT:%'[%1  ZWRITE @%
 QUIT
 ;
zwriteAIMIndex ; Write the index entries of every ^%ydbAIMD and ^%ydbAIMS global, and nothing else
 ; An index entry lives at (piece,value,key...), so it has at least three subscripts;
 ; AIM's metadata (build status, triggers, comments, ...) has at most two. The metadata
 ; carries a build timestamp and so differs between two builds of the same data by
 ; construction, which is why verify_aim_rebuild (tests/test_helpers.bash.in) compares
 ; the entries alone. The format is ZWRITE's: $QUERY supplies the subscripts and $ZWRITE
 ; renders the value.
 ; The ^%ydbAIMS globals of subscript cross references (XREFSUB, the primary key columns of YDBOcto#660)
 ; have their entries at (snum,value[,subscript...]): at least two subscripts, the first positive. Their
 ; metadata has one subscript, and their statistics are under -snum.
 NEW gbl,pfx,q
 SET (gbl,pfx)="^%ydbAIMD"
 FOR  SET gbl=$ORDER(@gbl) QUIT:gbl'[pfx  DO
 . SET q=gbl FOR  SET q=$QUERY(@q) QUIT:""=q  WRITE:2<$QLENGTH(q) q,"=",$ZWRITE(@q),!
 SET (gbl,pfx)="^%ydbAIMS"
 FOR  SET gbl=$ORDER(@gbl) QUIT:gbl'[pfx  DO
 . SET q=gbl FOR  SET q=$QUERY(@q) QUIT:""=q  WRITE:(1<$QLENGTH(q))&(0<$QSUBSCRIPT(q,1)) q,"=",$ZWRITE(@q),!
 QUIT
