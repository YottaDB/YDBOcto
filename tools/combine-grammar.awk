#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

match($0, /%include ".*"/) {
  fileName = path substr($0, RSTART+10, RLENGTH-11);
  while(( getline line<fileName) > 0 ) {
     print line
  }
}

! /^%include/ { print $0 }
