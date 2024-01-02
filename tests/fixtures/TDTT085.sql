#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select date(zut) '12265494700000000';
select timestamp(zut) '12265494700000000';
select date(zhorolog) '189078,,,';
select date(zhorolog) '189078,,,' = date(zut) '12265494700000000'; -- TRUE
select date_to_zhorolog(date(zut) '12265494700000000');
select date_to_zut(date(zhorolog) '189078,,,');
select date_to_zut(date(zut)'12265494700000000');
