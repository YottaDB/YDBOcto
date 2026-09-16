;################################################################
;#								#
;# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
;# All rights reserved.						#
;#								#
;#	This source code contains the intellectual property	#
;#	of its copyright holder(s), and is made available	#
;#	under a license.  If you do not know the terms of	#
;#	the license, please stop and do not read further.	#
;#								#
;################################################################
TC090	; YDBOcto#1149 : the "mykeys" label below ends in "keys", so "mykeys(" must not match "keys("
	quit
mykeys(id)
	quit "mk-"_id
