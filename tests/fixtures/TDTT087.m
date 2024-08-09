;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TDTT087	;
	; This test takes a randomly generated date/time/timestamp literal.
	; Finds its equivalent in different formats (fileman/horolog etc.).
	; Sets these literals as node values in different globals.
	; Does this 10 times so each global has 10 nodes.
	; Maps these globals to different READONLY tables.
	; Each table defines the column as date, date(fileman), date(horolog) etc. type.
	; It then generates queries to do JOIN on those tables with an ON condition that checks for the timestamp column values to be equal across the tables.
	; Since it is possible the randomly generated date/time/timestamp values can in rare cases match, the test expects at least as many (cannot be equal to) rows of values returned as the rows in the tables (i.e. >= 10).
	; And because the select column list of the query is the condition count(joined rows) >= 10, we expect a value of 1 to show up in each of the 4 randomly generated JOIN queries.
	; And that is part of the reference file.
	do init
	for i=1:1:numRows  do
	. set date=$$genDate
	. set dateFileman=$$genDateFileman
	. set dateHorolog=$$genDateHorolog
	. set dateZHorolog=$$genDateZHorolog
	. set dateZUT=$$genDateZUT
	. set time=$$genTime
	. set timeHorolog=$$genTimeHorolog
	. set timeWithTimeZone=$$genTimeWithTimeZone
	. set timeWithTimeZoneZHorolog=$$genTimeWithTimeZoneZHorolog
	. set timeZHorolog=$$genTimeZHorolog
	. set timestamp=$$genTimestamp
	. set timestampFileman=$$genTimestampFileman
	. set timestampHorolog=$$genTimestampHorolog
	. set timestampWithTimeZone=$$genTimestampWithTimeZone
	. set timestampWithTimeZoneZHorolog=$$genTimestampWithTimeZoneZHorolog
	. set timestampZHorolog=$$genTimestampZHorolog
	. set timestampZUT=$$genTimestampZUT
	. write "select date '"_date_"' as date_;",!
	. write "select date_to_fileman(date '"_date_"');",!
	. write "select date_to_horolog(date '"_date_"');",!
	. write "select date_to_zhorolog(date '"_date_"');",!
	. write "select date_to_zut(date '"_date_"');",!
	. ; write "select date(fileman) '"_dateFileman_"';",!
	. ; write "select date(horolog) '"_dateHorolog_"';",!
	. ; write "select date(zhorolog) '"_dateZHorolog_"';",!
	. ; write "select date(zut) '"_dateZUT_"';",!
	. ;
	. write "select time '"_time_"' as time_;",!
	. write "select time_to_horolog(time '"_time_"');",!
	. write "select time_to_zhorolog(time '"_time_"');",!
	. ; write "select time(horolog) '"_timeHorolog_"';",!
	. ; write "select time(zhorolog) '"_timeZHorolog_"';",!
	. ;
	. write "select time with time zone '"_timeWithTimeZone_"' as timetz_;",!
	. write "select timetz_to_horolog(time with time zone '"_timeWithTimeZone_"');",!
	. write "select timetz_to_zhorolog(time with time zone '"_timeWithTimeZone_"');",!
	. ; write "select time(zhorolog) with time zone '"_timeWithTimeZoneZHorolog_"';",!
	. ;
	. write "select timestamp '"_timestamp_"' as timestamp_;",!
	. write "select timestamp_to_fileman(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_horolog(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_zhorolog(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_zut(timestamp '"_timestamp_"');",!
	. ; write "select timestamp(fileman) '"_timestampFileman_"';",!
	. ; write "select timestamp(horolog) '"_timestampHorolog_"';",!
	. ; write "select timestamp(zhorolog) '"_timestampZHorolog_"';",!
	. ; write "select timestamp(zut) '"_timestampZUT_"';",!
	. ;
	. write "select timestamp with time zone '"_timestampWithTimeZone_"' as timestamptz_;",!
	. write "select timestamptz_to_fileman(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. write "select timestamptz_to_horolog(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. write "select timestamptz_to_zhorolog(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. ; write "select timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"';",!
	quit

genDate();
	; 1840-01-01 to 2699-12-31
	set month=$$genNum($random(12)+1,2)
	set day=$$genNum($random(31)+1,2)
	set year=1840+$$genNum($random(859+1),4)
	set date=year_"-"_month_"-"_day
	quit date
genTime();
	; 00:00:00.000000 to 23:59:59.999999
	set hour=$$genHour
	set minute=$$genMinute
	set second=$$genSecond
	set microsecond=$$genMicroSecond
	set time=$$getSeparator_hour_":"_minute_":"_second_microsecond
	quit time
genTimeWithTimeZone();
	; 00:00:00.000000-15:59 to 23:59:59.999999+15:59
	quit $$genTime_$$genTimeZone
genTimestamp();
	; 0000-01-01 00:00:00.000000 to 9999-12-31 23:59:59.999999
	quit $$genDate_$$genTime
genTimestampWithTimeZone();
	; 0000-01-01 00:00:00.000000-15:59 to 9999-12-31 23:59:59.999999+15:59 (01-JAN-0000 00:00:00.000000-15:59 to 31-DEC-9999 23:59:59.999999+15:59)
	quit $$genTimestamp_$$genTimeZone

genDateHorolog();
	; -365 (01-JAN-1840) to 313743 (31-DEC-2699)
	quit $random(313743+365+1)-365
genTimeHorolog();
	; 0 (00:00:00) to 86399 (23:59:59)
	quit $random(86399+1)
genTimestampHorolog();
	; -365,0 (01-JAN-1840 00:00:00) to 313743,86399 (31-DEC-2699 23:59:59)
	quit $$genDateHorolog_","_$$genTimeHorolog

genDateFileman();
	; 0010101 (01-JAN-1840) to 9991231 (31-DEC-2699)
	quit $$genDateOnlyFileman
genTimestampFileman();
	; 0010101.000000 (01-JAN-1701 00:00:00) to 9991231.235959 (31-DEC-2699 23:59:59)
	quit:'$random(100) $$genDateOnlyFileman
	quit $$genDateOnlyFileman_"."_$$genHour_$$genMinute_$select($random(2):$$genSecond,1:"")

genDateZHorolog();
	; -365,,, to 313743,,, (01-JAN-1840 to 31-DEC-9999)
	quit $$genDateOnlyZHorolog_",,,"
genTimeZHorolog();
	; ,0,[0], to ,86399,[999999], (00:00:00.00000) to 23:59:59.999999)
	quit ","_$$genSecondOnlyZHorolog_","_$$genZHorologMicroSecond_","
genTimeWithTimeZoneZHorolog();
	; ,0,[0],43200 to ,86399,[999999],-50400 (00:00:00.00000-12:00) to 23:59:59.999999+14:00)
	quit ","_$$genSecondOnlyZHorolog_","_$$genZHorologMicroSecond_","_$$genTimeZoneOnlyZHorolog
genTimestampZHorolog();
	; -365,0,[0], to 313743,86399,[999999], (01-JAN-1840 00:00:00.000000 to 31-DEC-9999 23:59:59.999999)
	quit $$genDateOnlyZHorolog_","_$$genSecondOnlyZHorolog_","_$$genZHorologMicroSecond_","
genTimestampWithTimeZoneZHorolog();
	; -365,0,[0],43200 to 313743,86399,[999999],-50400 (01-JAN-1840 00:00:00.000000-12:00 to 31-DEC-9999 23:59:59.999999+14:00)
	quit $$genDateOnlyZHorolog_","_$$genSecondOnlyZHorolog_","_$$genZHorologMicroSecond_","_$$genTimeZoneOnlyZHorolog

genDateZUT();
	; -4102444800000000 (1840-01-01) to 23036486400000000 (2699-12-31)
	quit $$genDateOnlyZUT_"00000000"
genTimestampZUT();
	; -4102444800000000 (1840-01-01 00:00:00.000000) to 253402300799999999(2699-12-31 23:59:59.999999)
	quit $$genDateOnlyZUT_$$genNum($random(100000000),8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions
getSeparator()
	quit $select($random(2)=1:"T",1:"t")

genTimeZone();
	; -15:59 to +15:59
	set sign=$select($random(2):"+",1:"-")
	set tzhour=$$genNum($random(16),2)
	set tzminute=$$genNum($random(60),2)
	quit sign_tzhour_":"_tzminute
assert(cond)	;
	if 'cond zshow "*"  zhalt 1
	quit

genNum(num,numDigits)
	set numlen=$length(num)
	do assert(numlen<=numDigits)
	quit $translate($justify(num,numDigits)," ",0)

genMicroSecond()
	quit ""

genDateOnlyZHorolog()
	quit $random(313743+365+1)-365

genSecondOnlyZHorolog()
	quit $random(86399+1)

genTimeZoneOnlyZHorolog()
	quit $random(50400+43200+1)-50400

genZHorologMicroSecond()
	quit $translate($$genMicroSecond,".")

genDateOnlyFileman()
	quit (140+$$genNum($random(999-140)+1,3))_$$genNum($random(12)+1,2)_$$genNum($random(31)+1,2)

genHour()
	quit $$genNum($random(24),2)

genMinute()
	quit $$genNum($random(60),2)

genSecond()
	quit $$genNum($random(60),2)

genDateOnlyZUT()
	quit $random(230364864+1+41024448)-41024448

genOctoqueries	;
	do init
	write "set datestyle=""ymd"";",!
	;
	set cnt=0
	set tableNames("date",$incr(cnt))="date_tbl"
	set tableNames("date",$incr(cnt))="date_fileman_tbl"
	set tableNames("date",$incr(cnt))="date_horolog_tbl"
	set tableNames("date",$incr(cnt))="date_zhorolog_tbl"
	set tableNames("date",$incr(cnt))="date_zut_tbl"
	;
	set cnt=0
	set tableNames("time",$incr(cnt))="time_tbl"
	set tableNames("time",$incr(cnt))="time_horolog_tbl"
	set tableNames("time",$incr(cnt))="time_zhorolog_tbl"
	;
	set cnt=0
	set tableNames("timetz",$incr(cnt))="timetz_tbl"
	set tableNames("timetz",$incr(cnt))="timetz_horolog_tbl"
	set tableNames("timetz",$incr(cnt))="timetz_zhorolog_tbl"
	;
	set cnt=0
	set tableNames("timestamp",$incr(cnt))="timestamp_tbl"
	set tableNames("timestamp",$incr(cnt))="timestamp_fileman_tbl"
	set tableNames("timestamp",$incr(cnt))="timestamp_horolog_tbl"
	set tableNames("timestamp",$incr(cnt))="timestamp_zhorolog_tbl"
	set tableNames("timestamp",$incr(cnt))="timestamp_zut_tbl"
	;
	set cnt=0
	set tableNames("timestamptz",$incr(cnt))="timestamptz_tbl"
	set tableNames("timestamptz",$incr(cnt))="timestamptz_fileman_tbl"
	set tableNames("timestamptz",$incr(cnt))="timestamptz_horolog_tbl"
	set tableNames("timestamptz",$incr(cnt))="timestamptz_zhorolog_tbl"
	;
	set columnName("date")="order_date"
	set columnName("time")="order_time"
	set columnName("timetz")="order_timetz"
	set columnName("timestamp")="order_timestamp"
	set columnName("timestamptz")="order_timestamptz"
	;
	set type="" for  set type=$order(tableNames(type))  quit:type=""  do
	. set max=$order(tableNames(type,""),-1)
	. for i=1:1:max do
	. . set typename=$piece(type,"tz",1)
	. . set istztype=$find(type,"tz")
	. . set format=$piece($piece($piece(tableNames(type,i),type,2),"_tbl",1),"_",2)
	. . write "create table "_tableNames(type,i)_" (order_id integer primary key, "
	. . write columnName(type)_" "_typename_$select(format="":"",1:"("_format_")")_$select(istztype:" with time zone",1:"")_") GLOBAL ""^"
	. . write $translate(tableNames(type,i),"_")_""" READONLY;",!
	. . ; write "select count(*) from "_tableNames(type,i)_";",!
	. set index1=1+$random(max)
	. set index2=1+$random(max)
	. write "select count(*) >= "_numRows_" from "_tableNames(type,index1)_" t1 inner join "_tableNames(type,index2)_" t2 on t1."_columnName(type)_" = t2."_columnName(type)_";",!
	quit

init	;
	set numRows=10
	quit

