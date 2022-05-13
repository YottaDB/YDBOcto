#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select distinct greatest('soml','loms','mosl') FROM names ORDER BY greatest('soml','loms','mosl');
select distinct least('soml','loms','mosl') FROM names ORDER BY least('soml','loms','mosl');
select distinct nullif('soml','loms') FROM names ORDER BY nullif('soml','loms');
select distinct coalesce('soml','loms','mosl') FROM names ORDER BY coalesce('soml','loms','mosl');
