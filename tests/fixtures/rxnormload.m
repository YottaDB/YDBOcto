 ;################################################################
 ;								#
 ; Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
 ; All rights reserved.						#
 ;								#
 ;	This source code contains the intellectual property	#
 ;	of its copyright holder(s), and is made available	#
 ;	under a license.  If you do not know the terms of	#
 ;	the license, please stop and do not read further.	#
 ;								#
 ;################################################################
rxnormload ; Routine to load RxNorm RRF files into globals
 quit
 ;
rxnconso ;
 new file set file=$ZCMDLINE
 open file use file
 kill ^RXNCONSO
 new i for i=1:1 read line quit:$zeof  set ^RXNCONSO(i)=line
 close file
 quit
 ;
rxnsat ;
 new file set file=$ZCMDLINE
 open file use file
 kill ^RXNSAT
 new i for i=1:1 read line quit:$zeof  set ^RXNSAT(i)=line
 close file
 quit
 ;
rxnrel ;
 new file set file=$ZCMDLINE
 open file use file
 kill ^RXNREL
 new i for i=1:1 read line quit:$zeof  set ^RXNREL(i)=line
 close file
 quit
