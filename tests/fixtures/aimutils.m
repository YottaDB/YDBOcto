aimutils ; Utilites for testing YDBAIM in Octo
 ;
 ; Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.
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
