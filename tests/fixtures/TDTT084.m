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

TDTT084	;
	for i=1:1:10  do
	. ;
	. set date=$$genDate
	. write "select date '"_date_"';",!
	. write "select DAY(date '"_date_"');",!
	. write "select DAYOFMONTH(date '"_date_"');",!
	. write "select date_to_fileman(date '"_date_"');",!
	. write "select date_to_horolog(date '"_date_"');",!
	. write "select date_to_zhorolog(date '"_date_"');",!
	. write "select date_to_zut(date '"_date_"');",!
	. ;
	. set time=$$genTime
	. write "select time '"_time_"';",!
	. write "select time_to_horolog(time '"_time_"');",!
	. write "select time_to_zhorolog(time '"_time_"');",!
	. ;
	. set timeWithTimeZone=$$genTimeWithTimeZone
	. write "select time with time zone '"_timeWithTimeZone_"';",!
	. write "select timetz_to_horolog(time with time zone '"_timeWithTimeZone_"');",!
	. ;
	. set timestamp=$$genTimestamp
	. write "select timestamp '"_timestamp_"';",!
	. write "select timestamp_to_fileman(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_horolog(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_zhorolog(timestamp '"_timestamp_"');",!
	. write "select timestamp_to_zut(timestamp '"_timestamp_"');",!
	. ;
	. set timestampWithTimeZone=$$genTimestampWithTimeZone
	. write "select timestamp with time zone '"_timestampWithTimeZone_"';",!
	. write "select timestamptz_to_fileman(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. write "select timestamptz_to_horolog(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. write "select timestamptz_to_zhorolog(timestamp with time zone '"_timestampWithTimeZone_"');",!
	. ;
	. set dateHorolog=$$genDateHorolog
	. write "select date(horolog) '"_dateHorolog_"';",!
	. write "select DAY(date(horolog) '"_dateHorolog_"');",!
	. write "select DAYOFMONTH(date(horolog) '"_dateHorolog_"');",!
	. write "select date_to_fileman(date(horolog) '"_dateHorolog_"');",!
	. write "select date_to_horolog(date(horolog) '"_dateHorolog_"');",!
	. write "select date_to_zhorolog(date(horolog) '"_dateHorolog_"');",!
	. write "select date_to_zut(date(horolog) '"_dateHorolog_"');",!
	. ;
	. set timeHorolog=$$genTimeHorolog
	. write "select time(horolog) '"_timeHorolog_"';",!
	. write "select time_to_horolog(time(horolog) '"_timeHorolog_"');",!
	. write "select time_to_zhorolog(time(horolog) '"_timeHorolog_"');",!
	. ;
	. set timestampHorolog=$$genTimestampHorolog
	. write "select timestamp(horolog) '"_timestampHorolog_"';",!
	. write "select timestamp_to_fileman(timestamp(horolog) '"_timestampHorolog_"');",!
	. write "select timestamp_to_horolog(timestamp(horolog) '"_timestampHorolog_"');",!
	. write "select timestamp_to_zhorolog(timestamp(horolog) '"_timestampHorolog_"');",!
	. write "select timestamp_to_zut(timestamp(horolog) '"_timestampHorolog_"');",!
	. ;
	. set dateFileman=$$genDateFileman
	. write "select date(fileman) '"_dateFileman_"';",!
	. write "select DAY(date(fileman) '"_dateFileman_"');",!
	. write "select DAYOFMONTH(date(fileman) '"_dateFileman_"');",!
	. write "select date_to_fileman(date(fileman) '"_dateFileman_"');",!
	. write "select date_to_horolog(date(fileman) '"_dateFileman_"');",!
	. write "select date_to_zhorolog(date(fileman) '"_dateFileman_"');",!
	. write "select date_to_zut(date(fileman) '"_dateFileman_"');",!
	. ;
	. set timestampFileman=$$genTimestampFileman
	. write "select timestamp(fileman) '"_timestampFileman_"';",!
	. write "select timestamp_to_fileman(timestamp(fileman) '"_timestampFileman_"');",!
	. write "select timestamp_to_horolog(timestamp(fileman) '"_timestampFileman_"');",!
	. write "select timestamp_to_zhorolog(timestamp(fileman) '"_timestampFileman_"');",!
	. write "select timestamp_to_zut(timestamp(fileman) '"_timestampFileman_"');",!
	.
	. set dateZHorolog=$$genDateZHorolog
	. write "select date(zhorolog) '"_dateZHorolog_"';",!
	. write "select DAY(date(zhorolog) '"_dateZHorolog_"');",!
	. write "select DAYOFMONTH(date(zhorolog) '"_dateZHorolog_"');",!
	. write "select date_to_fileman(date(zhorolog) '"_dateZHorolog_"');",!
	. write "select date_to_horolog(date(zhorolog) '"_dateZHorolog_"');",!
	. write "select date_to_zhorolog(date(zhorolog) '"_dateZHorolog_"');",!
	. write "select date_to_zut(date(zhorolog) '"_dateZHorolog_"');",!
	. ;
	. set timeZHorolog=$$genTimeZHorolog
	. write "select time(zhorolog) '"_timeZHorolog_"';",!
	. write "select time_to_horolog(time(zhorolog) '"_timeZHorolog_"');",!
	. write "select time_to_zhorolog(time(zhorolog) '"_timeZHorolog_"');",!
	. ;
	. set timeWithTimeZoneZHorolog=$$genTimeWithTimeZoneZHorolog
	. write "select time(zhorolog) with time zone '"_timeWithTimeZoneZHorolog_"';",!
	. write "select timetz_to_horolog(time(zhorolog) with time zone '"_timeWithTimeZoneZHorolog_"');",!
	. ;
	. set timestampZHorolog=$$genTimestampZHorolog
	. write "select timestamp(zhorolog) '"_timestampZHorolog_"';",!
	. write "select timestamp_to_fileman(timestamp(zhorolog) '"_timestampZHorolog_"');",!
	. write "select timestamp_to_horolog(timestamp(zhorolog) '"_timestampZHorolog_"');",!
	. write "select timestamp_to_zhorolog(timestamp(zhorolog) '"_timestampZHorolog_"');",!
	. write "select timestamp_to_zut(timestamp(zhorolog) '"_timestampZHorolog_"');",!
	. write "select time_to_zhorolog(time '"_time_"');",!
	. ;
	. set timestampWithTimeZoneZHorolog=$$genTimestampWithTimeZoneZHorolog
	. write "select timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"';",!
	. write "select timestamptz_to_fileman(timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"');",!
	. write "select timestamptz_to_horolog(timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"');",!
	. write "select timestamptz_to_zhorolog(timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"');",!
	. ;
	. set dateZUT=$$genDateZUT
	. write "select date(zut) '"_dateZUT_"';",!
	. write "select DAY(date(zut) '"_dateZUT_"');",!
	. write "select DAYOFMONTH(date(zut) '"_dateZUT_"');",!
	. write "select date_to_fileman(date(zut) '"_dateZUT_"');",!
	. write "select date_to_horolog(date(zut) '"_dateZUT_"');",!
	. write "select date_to_zhorolog(date(zut) '"_dateZUT_"');",!
	. write "select date_to_zut(date(zut) '"_dateZUT_"');",!
	. ;
	. set timestampZUT=$$genTimestampZUT
	. write "select timestamp(zut) '"_timestampZUT_"';",!
	. write "select timestamp_to_fileman(timestamp(zut) '"_timestampZUT_"');",!
	. write "select timestamp_to_horolog(timestamp(zut) '"_timestampZUT_"');",!
	. write "select timestamp_to_zhorolog(timestamp(zut) '"_timestampZUT_"');",!
	. write "select timestamp_to_zut(timestamp(zut) '"_timestampZUT_"');",!
	. write "select time_to_zhorolog(time '"_time_"');",!
	. ;
	. set date1=$$genRandDate
	. set date2=$$genRandDate
	. set time1=$$genRandTime
	. set time2=$$genRandTime
	. set timestamp1=$$genRandTimestamp
	. set timestamp2=$$genRandTimestamp
	. set timeWithTimeZone1=$$genRandTimeWithTimeZone
	. set timestampWithTimeZone1=$$genRandTimestampWithTimeZone
	. set timestampWithTimeZone2=$$genRandTimestampWithTimeZone
	. ; Test expressions on DATE/TIME
	. write "select "_date1_" "_$$genPlusOrMinus_" "_time1_";",!
	. write "select "_date1_" - "_date2_";",!
	. write "select "_time1_" + "_date1_";",!
	. write "select "_date1_" + "_timeWithTimeZone1_";",!
	. write "select "_timeWithTimeZone1_" + "_date1_";",!
	. write "select "_date1_" "_$$genPlusOrMinus_" "_$$genInt_";",!
	. write "select "_$$genInt_" + "_date1_";",!
	. write "select "_timeWithTimeZone1_" "_$$genPlusOrMinus_" "_time1_";",!
	. write "select "_time1_" + "_timeWithTimeZone1_";",!
	. write "select "_time1_" + "_timestamp1_";",!
	. write "select "_timestamp1_" "_$$genPlusOrMinus_" "_time1_";",!
	. write "select "_time1_" + "_timestampWithTimeZone1_";",!
	. write "select "_timestampWithTimeZone1_" "_$$genPlusOrMinus_" "_time1_";",!
	. write "select "_time1_" + NULL;",!
	. write "select NULL + "_time1_";",!
	. write "select NULL + "_timestamp1_";",!
	. write "select "_timestamp1_" "_$$genPlusOrMinus_" NULL;",!
	. write "select "_date1_" - NULL;",!
	. write "select "_timeWithTimeZone1_" - NULL;",!
	. write "select "_date1_" - date(horolog) '"_dateHorolog_"';",!
	. write "select "_date1_" || 'abcd';",!
	. write "select 'abcd' || "_date1_";",!
	. write "select "_date1_" || NULL;",!
	. write "select NULL || "_date1_";",!
	. write "select "_time1_" || 'abcd';",!
	. write "select 'abcd' || "_time1_";",!
	. write "select "_time1_" || NULL;",!
	. write "select NULL || "_time1_";",!
	. write "select "_timeWithTimeZone1_" || 'abcd';",!
	. write "select 'abcd' || "_timeWithTimeZone1_";",!
	. write "select "_timeWithTimeZone1_" || NULL;",!
	. write "select NULL || "_timeWithTimeZone1_";",!
	. write "select "_timestamp1_" || 'abcd';",!
	. write "select 'abcd' || "_timestamp1_";",!
	. write "select "_timestamp1_" || NULL;",!
	. write "select NULL || "_timestamp1_";",!
	. write "select "_timestampWithTimeZone1_" || 'abcd';",!
	. write "select 'abcd' || "_timestampWithTimeZone1_";",!
	. write "select "_timestampWithTimeZone1_" || NULL;",!
	. write "select NULL || "_timestampWithTimeZone1_";",!
	.
	. set compareOp=$$compareOp
	. write "select "_date1_" "_$$compareOp_" "_date2_";",!
	. write "select "_time1_" "_$$compareOp_" "_time2_";",!
	. write "select "_timestamp1_" "_$$compareOp_" "_timestamp2_";",!
	. write "select "_timestampWithTimeZone1_" "_$$compareOp_" "_timestampWithTimeZone2_";",!
	. set oprnd1=$$getRandCompOperand
	. set oprnd2=$$getRandCompOperand
	. write "select "_oprnd1_" "_$$compareOp_" "_oprnd2_";",!
	. write "select "_oprnd1_" "_$$compareOp_" ANY(select "_oprnd2_");",!
	. write "select "_oprnd1_" "_$$compareOp_" ALL(select "_oprnd2_");",!
	. write "select "_oprnd1_" is null;",!
	. write "select "_oprnd1_" is not null;",!
	. write "select "_oprnd1_" in ("_oprnd1_","_oprnd2_");",!
	. write "select "_oprnd1_" not in ("_oprnd1_","_oprnd2_");",!
	. write "select "_oprnd1_" between "_oprnd1_" and "_oprnd2_";",!
	. write "select "_oprnd1_" not between "_oprnd1_" and "_oprnd2_";",!
	quit

getRandCompOperand();
	new rand
	set rand=$random(6)
	quit:rand=0 date1
	quit:rand=1 timestamp1
	quit:rand=2 timestampWithTimeZone1
	quit:rand=3 date2
	quit:rand=4 timestamp2
	quit:rand=5 timestampWithTimeZone2
	quit

compareOp();
	new rand
	set rand=$random(6)
	quit:rand=0 ">"
	quit:rand=1 ">="
	quit:rand=2 "<"
	quit:rand=3 "<="
	quit:rand=4 "="
	quit:rand=5 "!="

genRandDate();
	new rand
	set rand=$random(5)
	quit:rand=0 "date '"_date_"'"
	quit:rand=1 "date(horolog) '"_dateHorolog_"'"
	quit:rand=2 "date(zhorolog) '"_dateZHorolog_"'"
	quit:rand=3 "date(fileman) '"_dateFileman_"'"
	quit:rand=4 "date(zut) '"_dateZUT_"'"
	quit

genRandTime();
	new rand
	set rand=$random(3)
	quit:rand=0 "time '"_time_"'"
	quit:rand=1 "time(horolog) '"_timeHorolog_"'"
	quit:rand=2 "time(zhorolog) '"_timeZHorolog_"'"
	quit

genRandTimeWithTimeZone();
	new rand
	set rand=$random(2)
	quit:rand=0 "time with time zone '"_timeWithTimeZone_"'"
	quit:rand=1 "time(zhorolog) with time zone '"_timeWithTimeZoneZHorolog_"'"
	quit

genRandTimestamp();
	new rand
	set rand=$random(5)
	quit:rand=0 "timestamp '"_timestamp_"'"
	quit:rand=1 "timestamp(horolog) '"_timestampHorolog_"'"
	quit:rand=2 "timestamp(zhorolog) '"_timestampZHorolog_"'"
	quit:rand=3 "timestamp(fileman) '"_timestampFileman_"'"
	quit:rand=4 "timestamp(zut) '"_timestampZUT_"'"
	quit

genRandTimestampWithTimeZone();
	new rand
	set rand=$random(2)
	quit:rand=0 "timestamp with time zone '"_timestampWithTimeZone_"'"
	quit:rand=1 "timestamp(zhorolog) with time zone '"_timestampWithTimeZoneZHorolog_"'"
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
	set time=hour_":"_minute_":"_second_microsecond
	quit time
genTimeWithTimeZone();
	; 00:00:00.000000-15:59 to 23:59:59.999999+15:59
	quit $$genTime_$$genTimeZone
genTimestamp();
	; 0000-01-01 00:00:00.000000 to 9999-12-31 23:59:59.999999
	quit $$genDate_" "_$$genTime
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
	quit $select($random(2)=1:"",1:"."_$$genNum($random(1000000),6))

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

genInt()
	quit $random(32767)

genPlusOrMinus()
	quit $select($random(2)=1:"+",1:"-")

