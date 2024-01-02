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
		print " set ^cnt=0";
	}
	{
		if (1 == (NR % 20)) {
			printf " set ^datetbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (2 == (NR % 20)) {
			printf " set ^datefilemantbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (3 == (NR % 20)) {
			printf " set ^datehorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (4 == (NR % 20)) {
			printf " set ^datezhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (5 == (NR % 20)) {
			printf " set ^datezuttbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (6 == (NR % 20)) {
			printf " set ^timetbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (7 == (NR % 20)) {
			printf " set ^timehorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (8 == (NR % 20)) {
			printf " set ^timezhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (9 == (NR % 20)) {
			printf " set ^timetztbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (10 == (NR % 20)) {
			printf " set ^timetzhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (11 == (NR % 20)) {
			printf " set ^timetzzhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (12 == (NR % 20)) {
			printf " set ^timestamptbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (13 == (NR % 20)) {
			printf " set ^timestampfilemantbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (14 == (NR % 20)) {
			printf " set ^timestamphorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (15 == (NR % 20)) {
			printf " set ^timestampzhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (16 == (NR % 20)) {
			printf " set ^timestampzuttbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (17 == (NR % 20)) {
			printf " set ^timestamptztbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (18 == (NR % 20)) {
			printf " set ^timestamptzfilemantbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (19 == (NR % 20)) {
			printf " set ^timestamptzhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		} else if (0 == (NR % 20)) {
			printf " set ^timestamptzzhorologtbl($incr(^cnt))=\"%s\"\n", $0;
		}
	}
END	{}
