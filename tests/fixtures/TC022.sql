#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC022 : OCTO320 : Octo converts empty column values based on type when NOT NULL is specified in the DDL

SELECT * FROM nullnames;

SELECT * FROM nullnames n1 LEFT JOIN nullnamesb n2 ON n1.exempt = n2.exempt;

SELECT * FROM nullnames WHERE id > 0;
SELECT * FROM nullnames WHERE id <> 0;

SELECT * FROM nullnames WHERE id > 3;
SELECT * FROM nullnames WHERE id <> 3;

SELECT * FROM nullnames WHERE yearstenured > 0;
SELECT * FROM nullnames WHERE yearstenured <> 0;
SELECT * FROM nullnames WHERE yearstenured > 3;
SELECT * FROM nullnames WHERE yearstenured <> 3;

SELECT * FROM nullnamesb WHERE yearstenured > 0;
SELECT * FROM nullnamesb WHERE yearstenured <> 0;
SELECT * FROM nullnamesb WHERE yearstenured > 3;
SELECT * FROM nullnamesb WHERE yearstenured <> 3;
