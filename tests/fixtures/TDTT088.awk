#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

BEGIN	{
		print "set datestyle=\"ymd\";"
	}
	{
		if (1 == (NR % 20)) {
			printf "insert into date_tbl                 values (%s, date '%s');\n", NR, $0;
		} else if (2 == (NR % 20)) {
			printf "insert into date_fileman_tbl         values (%s, date(fileman) '%s');\n", NR, $0;
		} else if (3 == (NR % 20)) {
			printf "insert into date_horolog_tbl         values (%s, date(horolog) '%s');\n", NR, $0;
		} else if (4 == (NR % 20)) {
			printf "insert into date_zhorolog_tbl        values (%s, date(zhorolog) '%s');\n", NR, $0;
		} else if (5 == (NR % 20)) {
			printf "insert into date_zut_tbl             values (%s, date(zut) '%s');\n", NR, $0;
		} else if (6 == (NR % 20)) {
			printf "insert into time_tbl                 values (%s, time '%s');\n", NR, $0;
		} else if (7 == (NR % 20)) {
			printf "insert into time_horolog_tbl         values (%s, time(horolog) '%s');\n", NR, $0;
		} else if (8 == (NR % 20)) {
			printf "insert into time_zhorolog_tbl        values (%s, time(zhorolog) '%s');\n", NR, $0;
		} else if (9 == (NR % 20)) {
			printf "insert into timetz_tbl               values (%s, time with time zone '%s');\n", NR, $0;
		} else if (10 == (NR % 20)) {
			printf "insert into timetz_horolog_tbl       values (%s, time(horolog) with time zone '%s');\n", NR, $0;
		} else if (11 == (NR % 20)) {
			printf "insert into timetz_zhorolog_tbl      values (%s, TIME(ZHOrolog) with time zone '%s');\n", NR, $0;
		} else if (12 == (NR % 20)) {
			printf "insert into timestamp_tbl            values (%s, timestamp '%s');\n", NR, $0;
		} else if (13 == (NR % 20)) {
			printf "insert into timestamp_fileman_tbl    values (%s, timestamp(fileman) '%s');\n", NR, $0;
		} else if (14 == (NR % 20)) {
			printf "insert into timestamp_horolog_tbl    values (%s, timestamp(horolog) '%s');\n", NR, $0;
		} else if (15 == (NR % 20)) {
			printf "insert into timestamp_zhorolog_tbl   values (%s, timestamp(zhorolog) '%s');\n", NR, $0;
		} else if (16 == (NR % 20)) {
			printf "insert into timestamp_zut_tbl        values (%s, timestamp(zut) '%s');\n", NR, $0;
		} else if (17 == (NR % 20)) {
			printf "insert into timestamptz_tbl          values (%s, timestamp with time zone '%s');\n", NR, $0;
		} else if (18 == (NR % 20)) {
			printf "insert into timestamptz_fileman_tbl  values (%s, timestamp(fileman) with time zone '%s');\n", NR, $0;
		} else if (19 == (NR % 20)) {
			printf "insert into timestamptz_horolog_tbl  values (%s, timestamp(horolog) with time zone '%s');\n", NR, $0;
		} else if (0 == (NR % 20)) {
			printf "insert into timestamptz_zhorolog_tbl values (%s, timestamp(zhorolog) with time zone '%s');\n", NR, $0;
		}
	}
END	{}
