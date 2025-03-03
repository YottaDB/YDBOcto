;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023-2025 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Helper functions used by date and time related bats tests

createconcatfunctiontest()
	; CREATE FUNCTION concat(VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT
	; DATE
	; TIME
	; TIMESTAMP
	; TIMESTAMP WITH TIME ZONE
	new queryStart,queryEnd
	set queryStart="select concat("
	set queryEnd=");";
	set defaultType="VARCHAR"
	new filename
	set filename=$zcmdline
	open filename:(append)
	use filename
	new map
	set map("DATE")="date'2023-01-01'"
	set map("TIME")="time'01:01:01'"
	set map("TIMESTAMP")="timestamp'2023-01-01 01:01:01'"
	set map("TIMESTAMP WITH TIME ZONE")="timestamp with time zone'2023-01-01 01:01:01-05:00'"
	set map("VARCHAR")="'sample text'"
	for type="DATE","TIME","TIMESTAMP","TIMESTAMP WITH TIME ZONE" do
	. write queryStart
	. write map(type)_", "_map(defaultType)
	. write queryEnd,!
	. write queryStart
	. write map(defaultType)_", "_map(type)
	. write queryEnd,!
	; CREATE FUNCTION concat(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctofCONCAT;
	for type1="DATE","TIME","TIMESTAMP","TIMESTAMP WITH TIME ZONE" do
	. for type2="DATE","TIME","TIMESTAMP","TIMESTAMP WITH TIME ZONE","VARCHAR" do
	. . write queryStart
	. . write map(defaultType)_", "_map(type1)_", "_map(type2)
	. . write queryEnd,!
	. . write queryStart
	. . write map(type1)_", "_map(defaultType)_", "_map(type2)
	. . write queryEnd,!
	. . write queryStart
	. . write map(type1)_", "_map(type2)_", "_map(defaultType)
	. . write queryEnd,!
	close filename
	quit

largedb()
	new year,month,day
	set (year,month,day)=0
	set year=2000
	for i=0:1:10000 do
	. set year=(i#2000)+2000
	. set month=(i#12)+1
	. set day=(i#27)+1
	. set ^datetime(i)=year_"-"_month_"-"_day
	QUIT


samevalue(val)
	new year,month,day
	set year=$EXTRACT(val,0,3)
	set month=$EXTRACT(val,4,5)
	set day=$EXTRACT(val,6,7)
	QUIT (1700+year)_"-"_month_"-"_day

datetimeerrorglobal
	; horolog
	set ^datehorolog(0)=-366
	set ^datehorolog(1)=-365
	set ^datehorolog(2)=2980014
	set ^datehorolog(3)=2980013
	set ^datehorolog(4)=""

	set ^timehorolog(0)=-1
	set ^timehorolog(1)=0
	set ^timehorolog(2)=86400
	set ^timehorolog(3)=86399
	set ^timehorolog(4)=""

	set ^timewithtimezonehorolog(0)=0
	set ^timewithtimezonehorolog(0)=86399
	set ^timewithtimezonehorolog(0)=""

	set ^timestamphorolog(0)="-365,-1"
	set ^timestamphorolog(1)="-366,0"
	set ^timestamphorolog(2)="2980014,0"
	set ^timestamphorolog(3)="2980013,86400"
	set ^timestamphorolog(4)="-365,0"
	set ^timestamphorolog(5)="2980013,86399"
	set ^timestamphorolog(6)=""
	set ^timestamphorolog(7)="0,"
	set ^timestamphorolog(8)=",0"

	set ^timestampwithtimezonehorolog(0)="-365,0"
	set ^timestampwithtimezonehorolog(1)="2980013,86399"
	set ^timestampwithtimezonehorolog(2)=""

	; zhorolog
	set ^datezhorolog(0)="-366,,,"
	set ^datezhorolog(1)="-365,,,"
	set ^datezhorolog(2)="2980013,,,"
	set ^datezhorolog(3)="2980014,,,"
	set ^datezhorolog(4)=",,,"
	set ^datezhorolog(4)=""

	set ^timezhorolog(0)=",-1,,"
	set ^timezhorolog(1)=",0,,"
	set ^timezhorolog(2)=",0,-1,"
	set ^timezhorolog(3)=",0,0,"
	set ^timezhorolog(4)=",86399,,"
	set ^timezhorolog(5)=",86400,,"
	set ^timezhorolog(6)=",86399,999999,"
	set ^timezhorolog(7)=",86399,100000,"
	set ^timezhorolog(8)=",,,"
	set ^timezhorolog(9)=""

	set ^timewithtimezonezhorolog(0)=",0,,43201"
	set ^timewithtimezonezhorolog(1)=",-1,,43200"
	set ^timewithtimezonezhorolog(2)=",0,,43200"
	set ^timewithtimezonezhorolog(3)=",0,-1,43200"
	set ^timewithtimezonezhorolog(4)=",0,0,43200"
	set ^timewithtimezonezhorolog(5)=",86399,,-50400"
	set ^timewithtimezonezhorolog(6)=",86400,,-50400"
	set ^timewithtimezonezhorolog(7)=",86399,,-50401"
	set ^timewithtimezonezhorolog(8)=",86399,999999,-50400"
	set ^timewithtimezonezhorolog(9)=",86399,1000000,-50400"
	set ^timewithtimezonezhorolog(10)=""

	set ^timestampzhorolog(0)="-366,0,,"
	set ^timestampzhorolog(1)="-365,-1,,"
	set ^timestampzhorolog(2)="-365,0,,"
	set ^timestampzhorolog(3)="-365,0,-1,"
	set ^timestampzhorolog(4)="-365,0,0,"
	set ^timestampzhorolog(5)="2980013,86399,,"
	set ^timestampzhorolog(6)="2980013,86400,,"
	set ^timestampzhorolog(7)="2980013,86399,999999,"
	set ^timestampzhorolog(8)="2980013,86399,1000000,"
	set ^timestampzhorolog(9)=",,,"

	set ^timestampwithtimezonezhorolog(0)="-365,0,,43201"
	set ^timestampwithtimezonezhorolog(1)="-366,0,,43200"
	set ^timestampwithtimezonezhorolog(3)="-365,-1,,43200"
	set ^timestampwithtimezonezhorolog(4)="-365,0,,43200"
	set ^timestampwithtimezonezhorolog(5)="-365,0,-1,43200"
	set ^timestampwithtimezonezhorolog(6)="-365,0,0,43200"
	set ^timestampwithtimezonezhorolog(7)="2980013,86399,,-50400"
	set ^timestampwithtimezonezhorolog(8)="2980014,86399,,-50400"
	set ^timestampwithtimezonezhorolog(9)="2980013,86400,,-50400"
	set ^timestampwithtimezonezhorolog(10)="2980013,86399,,-50401"
	set ^timestampwithtimezonezhorolog(11)="2980013,86399,999999,-50400"
	set ^timestampwithtimezonezhorolog(12)="2980013,86399,999999,-50401"
	set ^timestampwithtimezonezhorolog(13)="2980013,86399,1000000,-50400"
	set ^timestampwithtimezonezhorolog(14)="2980013,86400,999999,-50400"
	set ^timestampwithtimezonezhorolog(15)="2980014,86399,999999,-50400"
	set ^timestampwithtimezonezhorolog(16)=",,,"

	; fileman
	set ^datefileman(0)="0000000"
	set ^datefileman(1)="0000101"
	set ^datefileman(2)="9991231"
	set ^datefileman(3)="10001231"
	set ^datefileman(4)="-9991231"
	set ^datefileman(5)="+9991231"
	set ^datefileman(6)=""

	set ^timefileman(0)=""
	set ^timefileman(1)="0"

	set ^timewithtimezonefileman(0)="0"
	set ^timewithtimezonefileman(1)=""

	set ^timestampfileman(0)="0000000.000000"
	set ^timestampfileman(1)="0000101.000000"
	set ^timestampfileman(2)="9991231.240000"
	set ^timestampfileman(3)="10001231.000000"
	set ^timestampfileman(4)="-9991231.000000"
	set ^timestampfileman(5)="+9991231.000000"
	set ^timestampfileman(6)="0000000"
	set ^timestampfileman(7)=""

	set ^timestampwithtimezonefileman(0)="0.0"
	set ^timestampwithtimezonefileman(1)="0."
	set ^timestampwithtimezonefileman(2)=""

	; zut
	set ^datezut(0)="-62167201439000000"
	set ^datezut(1)="-62167201438000000"
	set ^datezut(2)="-62167219300000000"
	set ^datezut(3)="-62135596800000000"
	set ^datezut(4)="253402232400000000"
	set ^datezut(5)="253402232500000000"
	set ^datezut(6)="253402214400000000"
	set ^datezut(7)="253402214500000000"
	set ^datezut(8)=""

	set ^timezut(0)="-2209057300000000"
	set ^timezut(1)="-2209057200000000"
	set ^timezut(2)="-2208970800000001"
	set ^timezut(3)="-2208970800000002"
	set ^timezut(4)=""

	set ^timewithtimezonezut(0)="0"
	set ^timewithtimezonezut(1)=""

	set ^timestampzut(0)="-62167201439000000"
	set ^timestampzut(1)="-62167201438000000"
	set ^timestampzut(2)="-62135596800000000"
	set ^timestampzut(3)="-62167219300000000"
	set ^timestampzut(4)="253402318799999999"
	set ^timestampzut(5)="253402318800000000"
	set ^timestampzut(6)="253402300799999999"
	set ^timestampzut(7)="253402300800000000"
	set ^timestampzut(8)=""

	set ^timestampwithtimezonezut(0)="0"
	set ^timestampwithtimezonezut(1)=""

	; text
	set ^datetext(0)="0000-01-01"
	set ^datetext(1)="0000-00-01"
	set ^datetext(2)="0000-01-00"
	set ^datetext(3)="1000-01-010"
	set ^datetext(4)="9999-12-31"
	set ^datetext(5)="0100-01-01"
	set ^datetext(6)="0010-01-01"
	set ^datetext(7)="0001-01-01"
	set ^datetext(8)=""

	set ^timetext(0)="00:00:00.000000"
	set ^timetext(1)="00:00:00"
	set ^timetext(2)="23:59:59.999999"
	set ^timetext(3)="23:59:59"
	set ^timetext(4)="23:59:59.1000000"
	set ^timetext(5)="24:00:00.000000"
	set ^timetext(6)="-01:00:00"
	set ^timetext(7)="01:-01:00"
	set ^timetext(8)="00:60:00"
	set ^timetext(9)="00:00:60"
	set ^timetext(10)=""


	set ^timewithtimezonetext(0)="00:00:00.000000-16:00"
	set ^timewithtimezonetext(1)="00:00:00.000000-15:59"
	set ^timewithtimezonetext(2)="00:00:00.000000+15:59"
	set ^timewithtimezonetext(3)="00:00:00.000000+16:00"
	set ^timewithtimezonetext(4)="23:59:59.999999-16:00"
	set ^timewithtimezonetext(5)="23:59:59.999999-15:59"
	set ^timewithtimezonetext(6)="23:59:59.999999+15:59"
	set ^timewithtimezonetext(7)="23:59:59.999999+16:00"
	set ^timewithtimezonetext(8)=""

	set ^timestamptext(0)="0000-01-01"
	set ^timestamptext(1)="0100-01-01"
	set ^timestamptext(2)="0010-01-01"
	set ^timestamptext(3)="0001-01-01"
	set ^timestamptext(4)="0000-01-01 00:00:00"
	set ^timestamptext(5)="0100-01-01 00:00:00"
	set ^timestamptext(6)="0010-01-01 00:00:00"
	set ^timestamptext(7)="0001-01-01 00:00:00"
	set ^timestamptext(8)="1000-01-010 00:00:00"
	set ^timestamptext(9)="0000-01-01 00:00:00.000000"
	set ^timestamptext(10)="9999-12-31 23:59:59.999999"
	set ^timestamptext(11)="9999-12-31 23:59:59"
	set ^timestamptext(12)="9999-12-31 23:59:60"
	set ^timestamptext(13)="0000-01-01 23:60:59"
	set ^timestamptext(14)="9999-12-31 23:59:59.1000000"
	set ^timestamptext(14)=""

	set ^timestampwithtimezonetext(0)="0000-01-01 00:00:00.000000-16:00"
	set ^timestampwithtimezonetext(1)="0000-01-01 00:00:00.000000-15:59"
	set ^timestampwithtimezonetext(2)="0000-01-01 00:00:00.000000+15:59"
	set ^timestampwithtimezonetext(3)="0000-01-01 00:00:00.000000+16:00"
	set ^timestampwithtimezonetext(4)="9999-12-31 23:59:59.999999-16:00"
	set ^timestampwithtimezonetext(5)="9999-12-31 23:59:59.999999-15:59"
	set ^timestampwithtimezonetext(6)="9999-12-31 23:59:59.999999+15:59"
	set ^timestampwithtimezonetext(7)="9999-12-31 23:59:59.999999+16:00"
	set ^timestampwithtimezonetext(8)=""
	; ISO 8601
	set ^timetextiso(0)="T00:00:00.000000"
	set ^timetextiso(1)="T00:00:00"
	set ^timetextiso(2)="T23:59:59.999999"
	set ^timetextiso(3)="T23:59:59"
	set ^timetextiso(4)="T23:59:59.1000000"
	set ^timetextiso(5)="T24:00:00.000000"
	set ^timetextiso(6)="-T01:00:00"
	set ^timetextiso(7)="T01:-01:00"
	set ^timetextiso(8)="T00:60:00"
	set ^timetextiso(9)="T00:00:60"
	set ^timetextiso(10)=""
	set ^timetextiso(11)="t00:00:00.000000"
	set ^timetextiso(12)="t00:00:00"
	set ^timetextiso(13)="t23:59:59.999999"
	set ^timetextiso(14)="t23:59:59"
	set ^timetextiso(15)="t23:59:59.1000000"
	set ^timetextiso(16)="t24:00:00.000000"
	set ^timetextiso(17)="-t01:00:00"
	set ^timetextiso(18)="t01:-01:00"
	set ^timetextiso(19)="t00:60:00"
	set ^timetextiso(20)="t00:00:60"


	set ^timewithtimezonetextiso(0)="T00:00:00.000000-16:00"
	set ^timewithtimezonetextiso(1)="T00:00:00.000000-15:59"
	set ^timewithtimezonetextiso(2)="T00:00:00.000000+15:59"
	set ^timewithtimezonetextiso(3)="T00:00:00.000000+16:00"
	set ^timewithtimezonetextiso(4)="T23:59:59.999999-16:00"
	set ^timewithtimezonetextiso(5)="T23:59:59.999999-15:59"
	set ^timewithtimezonetextiso(6)="T23:59:59.999999+15:59"
	set ^timewithtimezonetextiso(7)="T23:59:59.999999+16:00"
	set ^timewithtimezonetextiso(8)=""
	set ^timewithtimezonetextiso(9)="t00:00:00.000000-16:00"
	set ^timewithtimezonetextiso(10)="t00:00:00.000000-15:59"
	set ^timewithtimezonetextiso(11)="t00:00:00.000000+15:59"
	set ^timewithtimezonetextiso(12)="t00:00:00.000000+16:00"
	set ^timewithtimezonetextiso(13)="t23:59:59.999999-16:00"
	set ^timewithtimezonetextiso(14)="t23:59:59.999999-15:59"
	set ^timewithtimezonetextiso(15)="t23:59:59.999999+15:59"
	set ^timewithtimezonetextiso(16)="t23:59:59.999999+16:00"

	set ^timestamptextiso(0)="0000-01-01"
	set ^timestamptextiso(1)="0100-01-01"
	set ^timestamptextiso(2)="0010-01-01"
	set ^timestamptextiso(3)="0001-01-01"
	set ^timestamptextiso(4)="0000-01-01T00:00:00"
	set ^timestamptextiso(5)="0100-01-01T00:00:00"
	set ^timestamptextiso(6)="0010-01-01T00:00:00"
	set ^timestamptextiso(7)="0001-01-01T00:00:00"
	set ^timestamptextiso(8)="1000-01-010T00:00:00"
	set ^timestamptextiso(9)="0000-01-01T00:00:00.000000"
	set ^timestamptextiso(10)="9999-12-31T23:59:59.999999"
	set ^timestamptextiso(11)="9999-12-31T23:59:59"
	set ^timestamptextiso(12)="9999-12-31T23:59:60"
	set ^timestamptextiso(13)="0000-01-01T23:60:59"
	set ^timestamptextiso(14)="9999-12-31T23:59:59.1000000"
	set ^timestamptextiso(15)=""
	set ^timestamptextiso(16)="0000-01-01t00:00:00"
	set ^timestamptextiso(17)="0100-01-01t00:00:00"
	set ^timestamptextiso(18)="0010-01-01t00:00:00"
	set ^timestamptextiso(19)="0001-01-01t00:00:00"
	set ^timestamptextiso(20)="1000-01-010t00:00:00"
	set ^timestamptextiso(21)="0000-01-01t00:00:00.000000"
	set ^timestamptextiso(22)="9999-12-31t23:59:59.999999"
	set ^timestamptextiso(23)="9999-12-31t23:59:59"
	set ^timestamptextiso(24)="9999-12-31t23:59:60"
	set ^timestamptextiso(25)="0000-01-01t23:60:59"
	set ^timestamptextiso(26)="9999-12-31t23:59:59.1000000"

	set ^timestampwithtimezonetextiso(0)="0000-01-01T00:00:00.000000-16:00"
	set ^timestampwithtimezonetextiso(1)="0000-01-01T00:00:00.000000-15:59"
	set ^timestampwithtimezonetextiso(2)="0000-01-01T00:00:00.000000+15:59"
	set ^timestampwithtimezonetextiso(3)="0000-01-01T00:00:00.000000+16:00"
	set ^timestampwithtimezonetextiso(4)="9999-12-31T23:59:59.999999-16:00"
	set ^timestampwithtimezonetextiso(5)="9999-12-31T23:59:59.999999-15:59"
	set ^timestampwithtimezonetextiso(6)="9999-12-31T23:59:59.999999+15:59"
	set ^timestampwithtimezonetextiso(7)="9999-12-31T23:59:59.999999+16:00"
	set ^timestampwithtimezonetextiso(8)=""
	set ^timestampwithtimezonetextiso(9)="0000-01-01t00:00:00.000000-16:00"
	set ^timestampwithtimezonetextiso(10)="0000-01-01t00:00:00.000000-15:59"
	set ^timestampwithtimezonetextiso(11)="0000-01-01t00:00:00.000000+15:59"
	set ^timestampwithtimezonetextiso(12)="0000-01-01t00:00:00.000000+16:00"
	set ^timestampwithtimezonetextiso(13)="9999-12-31t23:59:59.999999-16:00"
	set ^timestampwithtimezonetextiso(14)="9999-12-31t23:59:59.999999-15:59"
	set ^timestampwithtimezonetextiso(15)="9999-12-31t23:59:59.999999+15:59"
	set ^timestampwithtimezonetextiso(16)="9999-12-31t23:59:59.999999+16:00"

	; edge cases
	set ^edgecase(0)="2024-01-01 01:01:01.7323"
	set ^edgecase(1)="2024-01-01 01:01:01.732300-05:00"
	set ^edgecase(2)="2024-01-01 01:01:01.73230-05:00"
	set ^edgecase(3)="2024-01-01 01:01:01.7323-05:00"
	; ISO 8601
	set ^edgecaseiso(0)="2024-01-01T01:01:01.7323"
	set ^edgecaseiso(1)="2024-01-01T01:01:01.732300-05:00"
	set ^edgecaseiso(2)="2024-01-01T01:01:01.73230-05:00"
	set ^edgecaseiso(3)="2024-01-01T01:01:01.7323-05:00"
	set ^edgecaseiso(4)="2024-01-01t01:01:01.7323"
	set ^edgecaseiso(5)="2024-01-01t01:01:01.732300-05:00"
	set ^edgecaseiso(6)="2024-01-01t01:01:01.73230-05:00"
	set ^edgecaseiso(7)="2024-01-01t01:01:01.7323-05:00"
	QUIT

datetimemglobal(needPrimaryKey)
	set map("date","",0)="2023-01-01";
	set map("date","",1)="2023-01-02";
	set map("time","",0)="01:01:01";
	set map("time","",1)="01:02:01";
	set map("timetz","",0)="01:01:01-05:00"
	set map("timetz","",1)="01:01:01-06:00"
	set map("timestamp","",0)="2023-01-01 01:01:01"
	set map("timestamp","",1)="2023-01-02 01:02:01"
	set map("timestamptz","",0)="2023-01-01 01:01:01-05:00"
	set map("timestamptz","",1)="2023-01-01 01:01:01-06:00"
	set map("date","fileman",0)="3230101"
	set map("date","fileman",1)="3230102"
	set map("time","fileman",0)="010101"
	set map("time","fileman",1)="010201"
	set map("timetz","fileman",0)="010101"
	set map("timetz","fileman",1)="010201"
	set map("timestamp","fileman",0)="3230101.010101"
	set map("timestamp","fileman",1)="3230102.010201"
	set map("timestamptz","fileman",0)="3230101.010101"
	set map("timestamptz","fileman",1)="3230102.010201"
	set map("date","horolog",0)="66475"
	set map("date","horolog",1)="66476"
	set map("time","horolog",0)="3661"
	set map("time","horolog",1)="3721"
	set map("timetz","horolog",0)="3661"
	set map("timetz","horolog",1)="3721"
	set map("timestamp","horolog",0)="66475,3661"
	set map("timestamp","horolog",1)="66476,3721"
	set map("timestamptz","horolog",0)="66475,3661"
	set map("timestamptz","horolog",1)="66476,3721"
	set map("date","zhorolog",0)="66475,,,"
	set map("date","zhorolog",1)="66476,,,"
	set map("time","zhorolog",0)=",3661,1,"
	set map("time","zhorolog",1)=",3721,1,"
	set map("timetz","zhorolog",0)=",3661,1,18000"
	set map("timetz","zhorolog",1)=",3721,1,18000"
	set map("timestamp","zhorolog",0)="66475,3661,1,18000"
	set map("timestamp","zhorolog",1)="66476,3721,1,18000"
	set map("timestamptz","zhorolog",0)="66475,3661,1,18000"
	set map("timestamptz","zhorolog",1)="66476,3721,1,18000"
	set map("date","zut",0)="1700250859368731"
	set map("date","zut",1)="1700250859368731"
	set map("time","zut",0)="1700250859368731"
	set map("time","zut",1)="1700250859368731"
	set map("timetz","zut",0)="1700250859368731"
	set map("timetz","zut",1)="1700250859368731"
	set map("timestamp","zut",0)="1700250859368731"
	set map("timestamp","zut",1)="1700250859368731"
	set map("timestamptz","zut",0)="1700250859368731"
	set map("timestamptz","zut",1)="1700250859368731"

	for type="date","time","timetz","timestamp","timestamptz" do
	. for format="","fileman","horolog","zhorolog","zut" do
	. . if $data(needPrimaryKey) do
	. . . ; date/time column as primary key
	. . . set global="^datetimep"_type_format
	. . . set @global@(map(type,format,0))="0"
	. . . set @global@(map(type,format,1))="1"
	. . else  do
	. . . set global="^datetime"_type_format
	. . . set @global@(0)=map(type,format,0)
	. . . set @global@(1)=map(type,format,1)
	zwr @global
	QUIT

; datetimedrop() copies the naming convention followed here. Both complement each other so any change to name here
; should be done to the other function also
datetimecreate(subtest)
	new val
	set val("date")="'2023-01-01'"
	set val("time")="'01:01:01'"
	set val("timestamp")="'2023-01-01 01:01:01'"
	set val("time with time zone")="'01:01:01+05:00'"
	set val("timestamp with time zone")="'2023-01-01 01:01:01+05:00'"
	for type="date","time","time with time zone","timestamp","timestamp with time zone" do
	. for format="" do
	. . write "-- type:"_type_" format:"_format,!
	. . set tableName=subtest_$translate(type," ","")_format
	. . set tableName1=$piece(tableName,"withtimezone")
	. . set tableName2=$piece(tableName,"withtimezone",2)
	. . set:tableName["withtimezone" tableName1=tableName1_"tz"
	. . set tableName=tableName1_tableName2
	. . ; write "select '-- "_"create table "_tableName_" (id integer, dob "_type_" "_format_");';",!
	. . write "create table "_tableName_" (id integer, dob "_type_" "_format_");",!
	. . write "insert into "_tableName_" values(1,"_type_val(type)_");",!
	. . write !
	. write !
	quit

datetimedrop(subtest)
	; for type="date","time","time with time zone","timestamp","timestamp with time zone" do
	for type="date","time","timestamp","timestamp with time zone" do
	. for format="" do
	. . write "-- type:"_type_" format:"_format,!
	. . set tableName=subtest_$translate(type," ","")_format
	. . set tableName1=$piece(tableName,"withtimezone")
	. . set tableName2=$piece(tableName,"withtimezone",2)
	. . set:tableName["withtimezone" tableName1=tableName1_"tz"
	. . set tableName=tableName1_tableName2
	. . write "drop table "_tableName_";",!
	. . write !
	. write !
	quit

printquery(op,query)
	; write "select '-- "_$translate(query,"'","""")_"';",!
	new opName
	set opName=$select("+"=op:"Add","-"=op:"Sub","*"=op:"Mult","/"=op:"Div","%"=op:"Mod","||"=op:"Concat","OR"=op:"Or","AND"=op:"And","IS"=op:"Is","IS NOT"=op:"IsNot","="=op:"Comparison","!="=op:"Comparison","<"=op:"Comparison",">"=op:"Comparison","<="=op:"Comparison",">="=op:"Comparison","LIKE"=op:"Like","SIMILAR TO"=op:"Similarto","ANY"=op:"Any","ALL"=op:"All","BETWEEN"=op:"Between","NOT BETWEEN"=op:"Notbetween","IN"=op:"In","NOT IN"=op:"Notin",1:"")
	if ""'=opName do
	. new filename
	. set filename="input"_opName_".sql"
	. open filename:(append)
	. use filename
	. write query,!
	. close filename
	else  write query,!
	QUIT

binaryoperationstatements
	; Advantages of this test
	; * Let us have a test query set with all combinations possible
	; * Initially this can be validated by running the same set in postgres and seeing we get errors where errors are expected
	;   and correct value where correct value is expected. We could use the python query splitter used by QG to split each query
	;   and run it against postgres to easily identify where expected output is not seen
	; * This will give a good base to validate any edge case or any new operation or any new type or any new value
	; * specific values can be tested by inserting it into the table below which we want to test with.
	; * Seems possible to include daylight saving and time zone info also in this test set
	; * auto-generated
	; * Since run using load_fixture and verify_output the test will be fast
	; * Since it is an exhaustive test, coverage will be good.
	; * Will not need too much effort to write this test
	; Disadvantage
	; * Huge test, probably 10,000 lines
	; * This is only for binary_STATEMENT others will also add some lines but not this much
	; * load_fixture and verify_output will be used to execute the queries that get generated by this test. Which relies on
	;   manual verification of results initially and during further updates
	; DO init
	; define table
	;   create table zzz (int id,date)
	;   insert ..
	;   create table zzz (int id,time)
	;   insert ..
	;   create table zzz (int id,timetz)
	;   insert ..
	;   create table zzz (int id,timestamp)
	;   insert ..
	;   create table zzz (int id,timstamptz)
	;   insert ..
	; ; FOR "text","fileman","horolog","zhorolog","zut" DO
	; FOR "text" DO
	; . ; create table zzz"text"
	; . FOR "date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal", "date_reference"
	;     , "time_reference", "timetz_reference", "timestamp_reference", "timestamptz_reference"  DO
	; . . FOR "date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal", "date_reference"
	;         ,"time_reference", "timetz_reference", "timestamp_reference", "timestamptz_reference"
	;	  ,"integer","string","numeric","NULL" DO
	; . . . FOR "+","-","*","%" DO
	; . . . . ;
	; . . . FOR "||" DO
	; . . . . ;
	; . . . FOR "OR","AND" DO
	; . . . . ;
	; . . . FOR "IS","IS NOT" DO
	; . . . . ;
	; . . . FOR "=","!=","<",">","<=",">=" DO
	; . . . . ;
	; . . . FOR "LIKE","SIMILAR TO" DO
	; . . . . ;
	; . . . FOR "ANY","ALL" DO
	; . . . . ;
	; . . . FOR "BETWEEN","NOT BETWEEN" DO
	; . . . . ;
	; . . . FOR "IN","NOT IN" DO
	; . . . . ;
	;
	; Add table definitions
	set subtest=$zcmdline
	new file
	set file="inputCreate.sql"
	open file
	use file
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	close file
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	set map("integer","text")=3
	set map("string","text")="'sample string'"
	set map("numeric","text")=3.3
	set map("NULL","text")="NULL"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference" DO
	. . ; FOR type2="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . FOR type2="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . . NEW table1,table2
	. . . SET (table1,table2)=""
	. . . NEW includeJoin,bothReference
	. . . SET (includeJoin,bothReference)=0
	. . . IF ((type1["reference")&(type2["reference")) DO
	. . . . SET bothReference=1
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . . SET table2=subtest_$piece(type2,"_")
	. . . IF ('bothReference&((type1["reference")!(type2["reference"))) DO
	. . . . SET includeJoin=1
	. . . . SET:(type1["reference") table1=subtest_$piece(type1,"_")
	. . . . SET:(type2["reference") table2=subtest_$piece(type2,"_")
	. . . NEW leftAlias,rightAlias
	. . . SET (leftAlias,rightAlias)=""
	. . . IF (bothReference) SET leftAlias="n1.",rightAlias="n2."
	. . . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:""),rightAlias=$select(table2'="":"n1.",1:"")
	. . . NEW query SET query=""
	. . . FOR op="+","-","*","/","%" DO
	. . . . set query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery(op,query)
	. . . FOR op="||" DO
	. . . . set query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery(op,query)
	. . . FOR op="OR","AND" DO
	. . . . SET query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery(op,query)
	. . . FOR op="=","!=","<",">","<=",">=" DO
	. . . . FOR op2="","ANY","ALL" DO
	. . . . . IF (""=op2) set query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . . ELSE  set query="select "_leftAlias_map(type1,"text")_" "_op_" "_op2_"(select "_map(type2,"text")_$select(table2'="":" from "_table2,1:"")_")"_$select(""'=table1:" from "_table1_" n1",1:"")_";"
	. . . . . DO printquery(op,query)
	. . . FOR op="LIKE","SIMILAR TO" DO
	. . . . SET query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery(op,query)
	. . . FOR op="BETWEEN","NOT BETWEEN" DO
	. . . . SET query="select "_leftAlias_map(type1,"text")_" "_op_" "_rightAlias_map(type2,"text")_" AND "_leftAlias_map(type1,"text")_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery(op,query)
	. . . FOR op="IN","NOT IN" DO
	. . . . FOR op2="","subquery" DO
	. . . . . IF (""=op2) SET query="select "_leftAlias_map(type1,"text")_" "_op_" ("_rightAlias_map(type2,"text")_","_leftAlias_map(type1,"text")_")"_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . . ELSE  SET query="select "_leftAlias_map(type1,"text")_" "_op_" (select "_rightAlias_map(type2,"text")_")"_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . . DO printquery(op,query)
	set file="inputDrop.sql"
	open file
	use file
	DO datetimedrop(subtest)
	close file
	QUIT

unaryoperationstatements
	;
	; +
	; -
	; NOT
	; EXISTS
	; Add table definitions
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference" DO
	. . NEW table1
	. . SET table1=""
	. . NEW includeJoin
	. . SET (includeJoin)=0
	. . IF (type1["reference") DO
	. . . SET table1=subtest_$piece(type1,"_")
	. . . SET includeJoin=1
	. . NEW leftAlias
	. . SET leftAlias=""
	. . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:"")
	. . NEW query SET query=""
	. . FOR op="+","-" DO
	. . . SET query="select "_op_leftAlias_map(type1,"text")_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . . DO printquery("",query)
	. . ; NOT
	. . SET query="select NOT "_leftAlias_map(type1,"text")_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . DO printquery("",query)
	. . ; EXISTS
	. . SET query="select EXISTS(select "_leftAlias_map(type1,"text")_$select(includeJoin:" from "_table1_" n1",1:"")_");"
	. . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

castoperationstatements
	;
	; ::
	; CAST
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	set map("integer","text")=3
	set map("string","text")="'sample string'"
	set map("numeric","text")=3.3
	set map("boolean","text")="true"
	set map("NULL","text")="NULL"
	set map("date_literal","type")="DATE"
	set map("time_literal","type")="TIME"
	; set map("timetz_literal","type")="TIME WITH TIME ZONE"
	set map("timestamp_literal","type")="TIMESTAMP"
	set map("timestamptz_literal","type")="TIMESTAMP WITH TIME ZONE"
	set map("integer","type")="INTEGER"
	set map("string","type")="VARCHAR"
	set map("numeric","type")="NUMERIC"
	set map("boolean","type")="BOOLEAN"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL","boolean" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL","boolean" DO
	. . ; FOR type2="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","integer","string","numeric","boolean" DO
	. . FOR type2="date_literal","time_literal","timestamp_literal","timestamptz_literal","integer","string","numeric","boolean" DO
	. . . quit:((type1'["literal")&(type1'["reference"))&((type2'["literal")&(type2'["reference"))
	. . . NEW table1
	. . . SET table1=""
	. . . NEW includeJoin
	. . . SET (includeJoin)=0
	. . . IF ((type1["reference")&(type2["reference")) DO
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . IF (type1["reference") DO
	. . . . SET includeJoin=1
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . NEW leftAlias
	. . . SET (leftAlias)=""
	. . . IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:"")
	. . . NEW query SET query=""
	. . . ; op="::"
	. . . SET query="select "_map(type1,"text")_"::"_map(type2,"type")_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . . DO printquery("",query)
	. . . ; op="CAST"
	. . . SET query="select CAST("_map(type1,"text")_" AS "_map(type2,"type")_")"_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

caseoperationstatements
	;
	; 2 types
	;
	; CASE value_expression
	; WHEN value_1 THEN result_1
	; WHEN value_2 THEN result_2
	; [WHEN ... ]
	; [ELSE result_n]
	; END
	; select case id when 1 then 1 else 999 end from names;
	;
	; CASE WHEN condition_expression_1 THEN result_1
	;      WHEN condition_expression_2 THEN result_2
	;      [WHEN ... ]
	;      [ELSE result_n]
	; END
	; select case when id=1 then 1 else 999 end from names;
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	set map("integer","text")=3
	set map("string","text")="'sample string'"
	set map("numeric","text")=3.3
	set map("NULL","text")="NULL"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . ; FOR type2="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . FOR type2="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . . quit:((type1'["literal")&(type1'["reference"))&((type2'["literal")&(type2'["reference"))
	. . . NEW table1,table2
	. . . SET (table1,table2)=""
	. . . NEW includeJoin,bothReference
	. . . SET (includeJoin,bothReference)=0
	. . . IF ((type1["reference")&(type2["reference")) DO
	. . . . SET bothReference=1
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . . SET table2=subtest_$piece(type2,"_")
	. . . IF ('bothReference&((type1["reference")!(type2["reference"))) DO
	. . . . SET includeJoin=1
	. . . . SET:(type1["reference") table1=subtest_$piece(type1,"_")
	. . . . SET:(type2["reference") table2=subtest_$piece(type2,"_")
	. . . NEW leftAlias,rightAlias
	. . . SET (leftAlias,rightAlias)=""
	. . . IF (bothReference) SET leftAlias="n1.",rightAlias="n2."
	. . . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:""),rightAlias=$select(table2'="":"n1.",1:"")
	. . . NEW query SET query=""
	. . . ; 1st case type
	. . . SET query="select CASE "_leftAlias_map(type1,"text")_" WHEN "_rightAlias_map(type2,"text")_" THEN "_leftAlias_map(type1,"text")_" ELSE "_rightAlias_map(type2,"text")_" END"_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . DO printquery("",query)
	. . . ; 2nd case type
	. . . SET query="select CASE WHEN "_leftAlias_map(type1,"text")_" = "_rightAlias_map(type2,"text")_" THEN "_leftAlias_map(type1,"text")_" ELSE "_rightAlias_map(type2,"text")_" END"_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

arrayoperationstatements
	;
	; array(oneval)
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference" DO
	. . NEW table1
	. . SET table1=""
	. . NEW includeJoin
	. . SET (includeJoin)=0
	. . IF (type1["reference") DO
	. . . SET table1=subtest_$piece(type1,"_")
	. . . SET includeJoin=1
	. . NEW leftAlias
	. . SET leftAlias=""
	. . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:"")
	. . NEW query SET query=""
	. . ; SET op="ARRAY"
	. . SET query="select ARRAY(select "_map(type1,"text")_")"_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . DO printquery("",query)
	. . SET query="select ARRAY(values ("_map(type1,"text")_"))"_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

aggregatefunctionstatements
	;
	; count(*),count(),sum(),avg(),min(),max()
	; ALL/DISTINCT usage is allowed in all but count(*)
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference" DO
	. . NEW table1
	. . SET table1=""
	. . NEW includeJoin
	. . SET (includeJoin)=0
	. . IF (type1["reference") DO
	. . . SET table1=subtest_$piece(type1,"_")
	. . . SET includeJoin=1
	. . NEW leftAlias
	. . SET leftAlias=""
	. . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:"")
	. . NEW query SET query=""
	. . FOR op="COUNT","SUM","AVG","MIN","MAX" DO
	. . . FOR op2="","DISTINCT ","ALL " DO
	. . . . ; include id below if there is a join so that aggregate(dob) doesn't result in error because of `id` column not in group by
	. . . . SET query="select "_op_"("_op2_map(type1,"text")_")"_$select(includeJoin:" from "_table1_" n1 group by n1.id",1:"")_";"
	. . . . DO printquery("",query)
	. . . . IF (includeJoin&(""=op2)&("COUNT"=op)) DO
	. . . . . SET query="select COUNT(*) from "_table1_" n1;"
	. . . . . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

cogtltnifunctionstatements
	;
	; coalesce(),greatest(),least(),null_if()
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	set map("integer","text")=3
	set map("string","text")="'sample string'"
	set map("numeric","text")=3.3
	set map("NULL","text")="NULL"
	FOR tmp="text" DO
	. ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference" DO
	. FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference" DO
	. . ; FOR type2="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . FOR type2="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL" DO
	. . . quit:((type1'["literal")&(type1'["reference"))&((type2'["literal")&(type2'["reference"))
	. . . NEW table1,table2
	. . . SET (table1,table2)=""
	. . . NEW includeJoin,bothReference
	. . . SET (includeJoin,bothReference)=0
	. . . IF ((type1["reference")&(type2["reference")) DO
	. . . . SET bothReference=1
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . . SET table2=subtest_$piece(type2,"_")
	. . . IF ('bothReference&((type1["reference")!(type2["reference"))) DO
	. . . . SET includeJoin=1
	. . . . SET:(type1["reference") table1=subtest_$piece(type1,"_")
	. . . . SET:(type2["reference") table2=subtest_$piece(type2,"_")
	. . . NEW leftAlias,rightAlias
	. . . SET (leftAlias,rightAlias)=""
	. . . IF (bothReference) SET leftAlias="n1.",rightAlias="n2."
	. . . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:""),rightAlias=$select(table2'="":"n1.",1:"")
	. . . NEW query SET query=""
	. . . FOR op="COALESCE","GREATEST","LEAST","NULLIF" DO
	. . . . SET query="select "_op_"("_leftAlias_map(type1,"text")_","_rightAlias_map(type2,"text")_")"_$select(bothReference:" from "_table1_" n1, "_table2_" n2",includeJoin:" from "_$select(table1'="":table1,1:table2)_" n1",1:"")_";"
	. . . . DO printquery("",query)
	DO datetimedrop(subtest)
	QUIT

userdefinedfunctionstatements(subtest)
	; create function samevalue(integer) returns integer as $$samevalue^functions;
	set subtest=$zcmdline
	DO datetimecreate(subtest) ; creates datetimecreate.sql
	; Setup literal and reference map
	new map
	set map("date_literal","text")="date'2023-01-01'"
	set map("time_literal","text")="time'01:01:00'"
	; set map("timetz_literal","text")="time with time zone'01:01:00-05:00'"
	set map("timestamp_literal","text")="timestamp'2023-01-01 01:01:00'"
	set map("timestamptz_literal","text")="timestamp with time zone'2023-01-01 01:01:00-05:00'"
	set map("date_reference","text")="dob"
	set map("time_reference","text")="dob"
	; set map("timetz_reference","text")="dob"
	set map("timestamp_reference","text")="dob"
	set map("timestamptz_reference","text")="dob"
	set map("integer","text")=3
	set map("string","text")="'sample string'"
	set map("numeric","text")=3.3
	set map("boolean","text")="true"
	set map("NULL","text")="NULL"
	set map("date_literal","type")="DATE"
	set map("time_literal","type")="TIME"
	; set map("timetz_literal","type")="TIME WITH TIME ZONE"
	set map("timestamp_literal","type")="TIMESTAMP"
	set map("timestamptz_literal","type")="TIMESTAMP WITH TIME ZONE"
	set map("integer","type")="INTEGER"
	set map("string","type")="VARCHAR"
	set map("numeric","type")="NUMERIC"
	set map("boolean","type")="BOOLEAN"
	for type="date","time","time with time zone","timestamp","timestamp with time zone" do
	. new str
	. set str="create function samevalue("_type_") returns "_type_" as $$samevalue^functions;"
	. do printquery("",str)
	. FOR tmp="text" DO
	. . ; FOR type1="date_literal","time_literal","timetz_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timetz_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL","boolean" DO
	. . FOR type1="date_literal","time_literal","timestamp_literal","timestamptz_literal","date_reference","time_reference","timestamp_reference","timestamptz_reference","integer","string","numeric","NULL","boolean" DO
	. . . NEW table1
	. . . SET table1=""
	. . . NEW includeJoin
	. . . SET (includeJoin)=0
	. . . IF (type1["reference") DO
	. . . . SET table1=subtest_$piece(type1,"_")
	. . . . SET includeJoin=1
	. . . NEW leftAlias
	. . . SET leftAlias=""
	. . . ELSE  IF (includeJoin) SET leftAlias=$select(table1'="":"n1.",1:"")
	. . . NEW query SET query=""
	. . . SET query="select samevalue("_map(type1,"text")_")"_$select(includeJoin:" from "_table1_" n1",1:"")_";"
	. . . do printquery("",query)
	. set str="drop function samevalue("_type_");"
	. do printquery("",str)

	DO datetimedrop(subtest)
	quit

basic(subtest)
	; Setup literal and reference map
	; "%m/%d/%Y","%m-%d-%Y","%Y/%m/%d","%Y-%m-%d","%d-%m-%Y","%d/%m/%Y"
	; "%H:%M:%S"
	new map
	set map("date_literal","text",1)="2023-10-17"
	set map("date_literal","text",2)="2023-10-16"
	QUIT

; 00:00 23:59
;

; Following routines generates 2 files
; 1. input.sql with all select of literals
; 2. input2.sql with create table and select of all the rows of the table
generateDateTimeValues
	; Timestamp With Time zone
	; Date                     : 0000-01-01 to 9999-12-31
	; Following code skips year 0000 as Postgres doesn't seem to recognize this year even though sql standard specifies
	; it to be valid.
	; Postgres treats 1 as 2001 (https://dba.stackexchange.com/questions/235295/why-does-year-0-become-year-2000)
	;   but Octo doesn't so ensure the year field is 4 digit with leading 0's.
	new isLeapYear
	new skip
	new lclD
	new maxYear
	new startYear
	new enableDifferentFormatReadOnlyTesting
	new format
	new queries set queries=$zcmdline
	new includeTime
	new doNotIncludeDate set doNotIncludeDate=0
	new includeTimezone
	set queries=queries*1000
	new randInt
	new type
	new timezone
	; select which type you want to test
	set randInt=$random(4); 0-3 date or timestamp or time or timestamp with time zone
	set type=$select((0=randInt):"date",(1=randInt):"timestamp",(2=randInt):"time",1:"timestamp with time zone")
	set includeTime=$select((0=randInt):0,1:1)
	set includeTimezone=$select(("timestamp with time zone"=type):1,1:0)
	set:("time"=type) doNotIncludeDate=1
	; select which format in addition to text to test
	set randInt=$random(4); 0-3 25% chance for each value
	set:doNotIncludeDate&randInt=0 randInt=1+$random(2); This is time, avoid fileman and zut format tests
	if (0=randInt) do
	. ; 25% of the time ensure values are selected such that fileman testing is possible
	. ; Date     : 0000101 (01-JAN-1701) to 9991231 (31-DEC-2699)
	. set maxYear=2699
	. set startYear=1701+$random(999) ; 1701 to 2699
	. set enableDifferentFormatReadOnlyTesting=1
	. set format="fileman"
	else  if (1=randInt) do
	. ; 25% of the time ensure values are selected such that horolog/zhorolog testing is possible
	. ; Horolog: Date     : -365 (01-JAN-1840) to 2980013 (31-DEC-9999)
	. ; Zhorolog: Date     : -365,,, to 2980013,,, (01-JAN-1840 to 31-DEC-9999)
	. set maxYear=9999
	. set startYear=1840+$random(8160) ;1840-9999
	. set enableDifferentFormatReadOnlyTesting=1
	. if (0=$random(2)) set format="horolog"
	. else  set format="zhorolog"
	else  do
	. ; 50% of the time test text input range and zut
	. ; Avoid zut if time type is being tested as zut doesn't allow this type
	. ; zut range matches text range so same computation for both
	. if (doNotIncludeDate!("timestamp with time zone"=type)) do
	. . set enableDifferentFormatReadOnlyTesting=0
	. else  do
	. . set enableDifferentFormatReadOnlyTesting=1
	. . set format="zut"
	. set startYear=$random(10) ;0-9
	. set startYear=startYear*1000 ;0,1000,2000,3000,4000,5000,6000,7000,8000,9000
	. set startYear=startYear+$random(1000) ; startYear + (a value in range 0 to 999)
	. set maxYear=9999
	new filename set filename="input.sql"
	open filename:(append)
	use filename
	for year=startYear:1:maxYear do  QUIT:0=queries
	. for month=1:1:12 do  QUIT:0=queries
	. . for day=1:1:31 do  QUIT:0=queries
	. . . ; Jan 31 Feb 28 (29) March 31 April 30 May 31 June 30 July 31 August 31 Sept 30 Oct 31 Nov 30 Dec 31
	. . . set lclD=day
	. . . set isLeapYear=0
	. . . set skip=0
	. . . if (0=(year#4)) do
	. . . . set isLeapYear=1
	. . . . if (0=(year#100)) do
	. . . . . if (0'=(year#400)) do
	. . . . . . set isLeapYear=0
	. . . . . else  do
	. . . . . . set isLeapYear=1
	. . . if (2=month) do
	. . . . if (29=lclD) do
	. . . . . if (1=isLeapYear) set skip=0
	. . . . . else  set skip=1
	. . . . else  if (30=lclD) set skip=1
	. . . if (31=lclD) do
	. . . . set skip=$select(2=month:1,4=month:1,6=month:1,9=month:1,11=month:1,1:1)
	. . . if (0=year) set skip=1
	. . . if (0=skip) do
	. . . . new lclyear set lclyear=year
	. . . . set length=4-$length(year)
	. . . . for i=1:1:length do
	. . . . . set lclyear="0"_lclyear
	. . . . new lclstr
	. . . . if (doNotIncludeDate) set lclstr=""
	. . . . else  set lclstr=lclyear_"-"_month_"-"_lclD
	. . . . write "select "_type_"'"; query begin
	. . . . ; Add time if type is timestamp or time
	. . . . ; HH:MM:SS.uuuuuu
	. . . . if (includeTime) do
	. . . . . new hour,minute,second,microsecond,time
	. . . . . set hour=$random(24) ; 0-23
	. . . . . set minute=$random(60) ; 0-59
	. . . . . set second=$random(60) ; 0-59
	. . . . . set time=hour_":"_minute_":"_second
	. . . . . set:(0=$random(3)) time=time_"."_$random(1000000) ; add microseconds 0-999999
	. . . . . set:('doNotIncludeDate) lclstr=lclstr_" "
	. . . . . set lclstr=lclstr_time
	. . . . if includeTimezone do
	. . . . . ; include time zone information as this is a timestamp with time zone
	. . . . . ; Choose the time zone value here
	. . . . . set randInt=$random(3) ; 0,1,2
	. . . . . if (0=randInt) do
	. . . . . . ; Pick +00:00 - +00:59
	. . . . . . set timezoneHour="+00"
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone=timezoneHour_":"_timezoneMinute
	. . . . . else  if (1=randInt) do
	. . . . . . ; Pick -12 to -1
	. . . . . . set value=$random(12)+1
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneHour=value
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone="-"_timezoneHour_":"_timezoneMinute
	. . . . . else  do
	. . . . . . ; Pick +1 to +14
	. . . . . . set value=$random(14)+1
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneHour=value
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone="+"_timezoneHour_":"_timezoneMinute
	. . . . . set lclstr=lclstr_timezone
	. . . . write lclstr,"';",! ; query end
	. . . . set ^date(queries)=lclstr ; global name is date for all types
	. . . . set queries=queries-1
	. . . quit:0=queries
	close filename
	set filename="input2.sql"
	open filename:(append)
	use filename
	write "create table test (id integer primary key, dob "_type_") global ""^date(keys(""""id""""))"" readonly;",! ; global name always is date
	write "select * from test;"
	close filename
	if (enableDifferentFormatReadOnlyTesting) do
	. ; Communicate to TDTT046 which format to test
	. set filename=format_".range"
	. open filename:(append)
	. use filename
	. write "Test values in "_format_" range",!
	. close filename
	. ; Communicate to TDTT046 which type to test
	. set filename=$select("timestamp with time zone"=type:"timestamp_with_time_zone",1:type)_".type"
	. open filename:(append)
	. use filename
	. write "Test values of "_type_" type",!
	. close filename
	. set filename="octo_"_format_"_readonly_table_query.sql"
	. open filename:(append)
	. use filename
	. write "create table test"_format_" (id integer primary key, dob "
	. write $select(includeTimezone:"timestamp"_"("_format_")"_" with time zone",1:type_"("_format_")")_") global ""^date"
	. write format_"(keys(""""id""""))"" readonly;",!  ;global name always starts with date TDTT046 depends on it
	. write "select * from test"_format_";"
	. close filename
	QUIT

; input.sql is generated by this routine
; create table query generated by this routine will have test1 and test2 tables
genCreateInsertSelectDateTimeValues
	; Following code skips year 0000 as Postgres doesn't seem to recognize this year even though sql standard specifies
	; it to be valid.
	; Postgres treats 1 as 2001 (https://dba.stackexchange.com/questions/235295/why-does-year-0-become-year-2000)
	;   but Octo doesn't so ensure the year field is 4 digit with leading 0's.
	new isLeapYear
	new skip
	new lclD
	new maxYear
	new startYear
	new enableDifferentFormatReadOnlyTesting
	new format
	new queries set queries=$zcmdline
	new includeTime
	new doNotIncludeDate set doNotIncludeDate=0
	new includeTimezone
	set queries=queries*1000
	new randInt
	new type
	new timezone
	; select which type you want to test
	set randInt=$random(4); 0-3 date or timestamp or time or timestamp with time zone
	set type=$select((0=randInt):"date",(1=randInt):"timestamp",(2=randInt):"time",1:"timestamp with time zone")
	set includeTime=$select((0=randInt):0,1:1)
	set includeTimezone=$select(("timestamp with time zone"=type):1,1:0)
	set:("time"=type) doNotIncludeDate=1
	; select which format in addition to text to test
	set randInt=$random(4); 0-3 25% chance for each value
	set:(doNotIncludeDate&(randInt=0)) randInt=1+$random(2); This is time, avoid fileman and zut format tests
	if (0=randInt) do
	. ; 25% of the time ensure values are selected such that fileman testing is possible
	. ; Date     : 0000101 (01-JAN-1701) to 9991231 (31-DEC-2699)
	. set maxYear=2699
	. set startYear=1701+$random(999) ; 1701 to 2699
	. set enableDifferentFormatReadOnlyTesting=1
	. set format="fileman"
	else  if (1=randInt) do
	. ; 25% of the time ensure values are selected such that horolog/zhorolog testing is possible
	. ; Horolog: Date     : -365 (01-JAN-1840) to 2980013 (31-DEC-9999)
	. ; Zhorolog: Date     : -365,,, to 2980013,,, (01-JAN-1840 to 31-DEC-9999)
	. set maxYear=9999
	. set startYear=1840+$random(8160) ;1840-9999
	. set enableDifferentFormatReadOnlyTesting=1
	. if (0=$random(2)) set format="horolog"
	. else  set format="zhorolog"
	else  do
	. ; 50% of the time test text input range and zut
	. ; Avoid zut if time type is being tested as zut doesn't allow this type
	. ; zut range matches text range so same computation for both
	. if ((doNotIncludeDate)!("timestamp with time zone"=type)) do
	. . set enableDifferentFormatReadOnlyTesting=0
	. else  do
	. . set enableDifferentFormatReadOnlyTesting=1
	. . set format="zut"
	. set startYear=$random(10) ;0-9
	. set startYear=startYear*1000 ;0,1000,2000,3000,4000,5000,6000,7000,8000,9000
	. set startYear=startYear+$random(1000) ; startYear + (a value in range 0 to 999)
	. set maxYear=9999
	new filename set filename="input.sql"
	open filename:(append)
	use filename
	; Write a DROP TABLE query first
	write "drop table if exists test1;",!
	; Write the CREATE TABLE query
	write "create table test1 (id integer primary key, dob "_type_");",! ; global name always is date
	; Write the INSERT TABLE queries
	for year=startYear:1:maxYear do  QUIT:0=queries
	. for month=1:1:12 do  QUIT:0=queries
	. . for day=1:1:31 do  QUIT:0=queries
	. . . ; Jan 31 Feb 28 (29) March 31 April 30 May 31 June 30 July 31 August 31 Sept 30 Oct 31 Nov 30 Dec 31
	. . . set lclD=day
	. . . set isLeapYear=0
	. . . set skip=0
	. . . if (0=(year#4)) do
	. . . . set isLeapYear=1
	. . . . if (0=(year#100)) do
	. . . . . if (0'=(year#400)) do
	. . . . . . set isLeapYear=0
	. . . . . else  do
	. . . . . . set isLeapYear=1
	. . . if (2=month) do
	. . . . if (29=lclD) do
	. . . . . if (1=isLeapYear) set skip=0
	. . . . . else  set skip=1
	. . . . else  if (30=lclD) set skip=1
	. . . if (31=lclD) do
	. . . . set skip=$select(2=month:1,4=month:1,6=month:1,9=month:1,11=month:1,1:1)
	. . . if (0=year) set skip=1
	. . . if (0=skip) do
	. . . . new lclyear set lclyear=year
	. . . . set length=4-$length(year)
	. . . . for i=1:1:length do
	. . . . . set lclyear="0"_lclyear
	. . . . new lclstr
	. . . . if (doNotIncludeDate) set lclstr=""
	. . . . else  set lclstr=lclyear_"-"_month_"-"_lclD
	. . . . write "insert into test1 values("_queries_","_type_"'"; query begin
	. . . . ; Add time if type is timestamp or time
	. . . . ; HH:MM:SS.uuuuuu
	. . . . if (includeTime) do
	. . . . . new hour,minute,second,microsecond,time
	. . . . . set hour=$random(24) ; 0-23
	. . . . . set minute=$random(60) ; 0-59
	. . . . . set second=$random(60) ; 0-59
	. . . . . set time=hour_":"_minute_":"_second
	. . . . . set:(0=$random(3)) time=time_"."_$random(1000000) ; add microseconds 0-999999
	. . . . . set:('doNotIncludeDate) lclstr=lclstr_$select(1=$random(2):" ",1:"T")
	. . . . . set:(doNotIncludeDate&(1=$random(2))) time="T"_time
	. . . . . set lclstr=lclstr_time
	. . . . if includeTimezone do
	. . . . . ; include time zone information as this is a timestamp with time zone
	. . . . . ; Choose the time zone value here
	. . . . . set randInt=$random(3) ; 0,1,2
	. . . . . if (0=randInt) do
	. . . . . . ; Pick +00:00 - +00:59
	. . . . . . set timezoneHour="+00"
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone=timezoneHour_":"_timezoneMinute
	. . . . . else  if (1=randInt) do
	. . . . . . ; Pick -12 to -1
	. . . . . . set value=$random(12)+1
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneHour=value
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone="-"_timezoneHour_":"_timezoneMinute
	. . . . . else  do
	. . . . . . ; Pick +1 to +14
	. . . . . . set value=$random(14)+1
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneHour=value
	. . . . . . set value=$random(60) ; 0-59
	. . . . . . set:(10>value) value="0"_value
	. . . . . . set timezoneMinute=value
	. . . . . . set timezone="+"_timezoneHour_":"_timezoneMinute
	. . . . . set lclstr=lclstr_timezone
	. . . . write lclstr,"');",! ; query end
	. . . . set ^date(queries)=lclstr ; global name is date for all types
	. . . . set queries=queries-1
	. . . quit:0=queries
	; Write the select query
	write "select * from test1 order by id;",!
	close filename
	if (enableDifferentFormatReadOnlyTesting) do
	. ; Communicate to TDTT049 which format to test
	. set filename=format_".range"
	. open filename:(append)
	. use filename
	. write "Test values in "_format_" range",!
	. close filename
	. ; Communicate to TDTT049 which type to test
	. set filename=$select("timestamp with time zone"=type:"timestamp_with_time_zone",1:type)_".type"
	. open filename:(append)
	. use filename
	. write "Test values of "_type_" type",!
	. close filename
	. set filename="input_format.sql"
	. open filename:(append)
	. use filename
	. ; Write a DROP TABLE query first
	. write "drop table if exists test2;",!
	. ; Write the CREATE TABLE query
	. if ("timestamp with time zone"=type)  write "create table test2 (id integer primary key, dob timestamp("_format_") with time zone);",!
	. else  write "create table test2 (id integer primary key, dob "_type_"("_format_"));",!
	. write "select * from test2 order by id;",!
	. close filename
	QUIT

getDate()
	new result
	new year,month,day
	; Pick values between 2000 to 9998.
	; This range helps to avoid
	; * Expression exceeding date/time boundaries
	; * Timezone with seconds information in Postgres. Following is a difference that occurs because of it.
	;   Octo doesn't support timezone seconds so skip this range.
	;   ```
	;   Query:
	;   select timestamp with time zone'3-15-0582 7:7:35+00:23' - time'11:10:51.356728';
	;
	;   Diff:
	;   < 0582-03-14 14:37:41.643272-04:56:02
	;   ---
	;   > 0582-03-14 14:37:43.643272-04:56
	;   ```
	set year=$random(8) ;0-7
	set year=year+2 ; 2-9
	set year=year*1000 ;0,1000,2000,3000,4000,5000,6000,7000,8000,9000
	set year=year+$random(1000) ; year + (a value in range 0 to 999)
	set:(9998<year) year=9998
	set length=4-$length(year)
	for i=1:1:length do
	. set year="0"_year
	; month
	set month=$random(12)+1 ; 1-12
	; day
	set day=$random(31)+1 ; 1-31
	; Jan 31 Feb 28 (29) March 31 April 30 May 31 June 30 July 31 August 31 Sept 30 Oct 31 Nov 30 Dec 31
	set lclD=day
	set isLeapYear=0
	set skip=0
	; is leap year?
	if (0=(year#4)) do
	. set isLeapYear=1
	. if (0=(year#100)) do
	. . if (0'=(year#400)) do
	. . . set isLeapYear=0
	. . else  do
	. . . set isLeapYear=1
	; is feb?
	if (2=month) do
	. if (29<=lclD) do
	. . if (1=isLeapYear) set lclD=29
	. . else  set lclD=28
	if (31=lclD) do
	. set lclD=$select(4=month:30,6=month:30,9=month:30,11=month:30,1:30)
	set day=lclD
	set result=year_"-"_month_"-"_day
	set result="date'"_result_"'"
	quit result

getTime()
	new result,hour,minute,second
	set hour=$random(24) ; 0-23
	set minute=$random(60) ; 0-59
	set second=$random(60) ; 0-59
	set result=hour_":"_minute_":"_second
	set:(0=$random(3)) result=result_"."_$random(1000000) ; add microseconds 0-999999
	set result="time'"_result_"'"
	quit result

getTimeZone()
	new result,timezoneHour,timezoneMinute,randInt,value
	set randInt=$random(3) ; 0,1,2
	if (0=randInt) do
	. ; Pick +00:00 - +00:59
	. set timezoneHour="+00"
	. set value=$random(60) ; 0-59
	. set:(10>value) value="0"_value
	. set timezoneMinute=value
	. set timezone=timezoneHour_":"_timezoneMinute
	else  if (1=randInt) do
	. ; Pick -12 to -1
	. set value=$random(12)+1
	. set:(10>value) value="0"_value
	. set timezoneHour=value
	. set value=$random(60) ; 0-59
	. set:(10>value) value="0"_value
	. set timezoneMinute=value
	. set timezone="-"_timezoneHour_":"_timezoneMinute
	else  do
	. ; Pick +1 to +14
	. set value=$random(14)+1
	. set:(10>value) value="0"_value
	. set timezoneHour=value
	. set value=$random(60) ; 0-59
	. set:(10>value) value="0"_value
	. set timezoneMinute=value
	. set timezone="+"_timezoneHour_":"_timezoneMinute
	set result=timezone
	quit result

getTimeTz()
	new result
	set result=$$getTime
	set result=$piece(result,"'",2) ; Get only the value in quotes
	set result="time with time zone'"_result
	set result=result_$$getTimeZone_"'"
	quit result

getTimestamp()
	new result,date,time
	set date=$$getDate
	set date=$piece(date,"'",2)
	set time=$$getTime
	set time=$piece(time,"'",2)
	set result="timestamp'"_date_" "_time_"'"
	quit result

getTimestampTz()
	new result
	set result=$$getDate
	set result=$piece(result,"'",2) ; Get only the value in quotes
	set result="timestamp with time zone'"_result_" "_$piece($$getTimeTz,"'",2)_"'"
	quit result

getInt()
	quit $random(1000)

getNull()
	quit "NULL"

createDateBoundaryIntegerValueMap()
	quit:(0'=$data(map))
	; Following maps are used by getAddExpression()
	; It is placed here to avoid recomputation everytime getAddExpression is invoked
	; Map of month and its days
	; Jan 31 Feb 28 (29) March 31 April 30 May 31 June 30 July 31 August 31 Sept 30 Oct 31 Nov 30 Dec 31
	set mmap(1)=31
	set mmap(2)=28
	set mmap(3)=31
	set mmap(4)=30
	set mmap(5)=31
	set mmap(6)=30
	set mmap(7)=31
	set mmap(8)=31
	set mmap(9)=30
	set mmap(10)=31
	set mmap(11)=30
	set mmap(12)=31
	; Map of random integer value which can be added to a date having year in the range 9997 to 9999
	; based on month and date
	for i=9997:1:9999 do
	. for j=1:1:12 do
	. . for k=1:1:31 do
	. . . ; i is year, j is month, k is day
	. . . set int=$$getInt
	. . . set dayOfMonth=mmap(j)
	. . . if dayOfMonth>=int do
	. . . . set int=dayOfMonth-int ; The value of int when added to day of Month will reach the last day of month
	. . . . set int=$random($select(0=int:1,1:int)) ; Choose a random value which is within the value of int
	. . . else  do
	. . . . ; The value of int is greater than number of days in a month
	. . . . set val=0
	. . . . if (i=9997) do
	. . . . . ; 365 in a year so 2 times this value can be added here easily
	. . . . . ; 730
	. . . . . for tmp=j+1:1:12 do
	. . . . . . set val=val+mmap(tmp)
	. . . . . set val=val+730 ; total number days that can be added from remaining years other than the present one(present month dates are ignored)
	. . . . else  if (i=9998) do
	. . . . . ; 365
	. . . . . for tmp=j+1:1:12 do
	. . . . . . set val=val+mmap(tmp)
	. . . . . set val=val+365 ; total number of days that can be added from remaining years other than the present one(present month dates are ignored)
	. . . . else  if (i=9999) do
	. . . . . for tmp=j+1:1:12 do
	. . . . . . set val=val+mmap(tmp)
	. . . . . set val=val+0 ; total number of days that can be added from remaining years other than the present one(present month dates are ignored)
	. . . . set int=$random($select(0=val:1,1:val))
	. . . set map(i,j,k)=int
	quit

getAddExpression()
	new randInt,result,int,val,tmp
	set randInt=$random(16)
	; TODO (#1044): Remove the following line after time with time zone is properly supported
	set randInt=$select(randInt=2:0,randInt=3:1,randInt=6:8,randInt=7:9,1:randInt)
	if (0=randInt) do
	. ; DATE + TIME
	. set result=$$getDate_" + "_$$getTime
	else  if (1=randInt) do
	. ; TIME + DATE
	. set result=$$getTime_" + "_$$getDate
	else  if (2=randInt) do
	. ; DATE + TIMEWITHTIMEZONE
	. set result=$$getDate_" + "_$$getTimeTz
	else  if (3=randInt) do
	. ; TIMEWITHTIMEZONE + DATE
	. set result=$$getTimeTz_" + "_$$getDate
	else  if (4=randInt)!(5=randInt) do
	. ; DATE + INTEGER
	. ; INTEGER + DATE
	. set date=$$getDate
	. set year=$piece(date,"-",1)
	. set year=$piece(year,"'",2)
	. set month=$piece(date,"-",2)
	. set day=$piece(date,"-",3)
	. set day=$piece(day,"'",1)
	. if (9996<+year) do createDateBoundaryIntegerValueMap set int=map(year,month,day)
	. else  set int=$$getInt
	. if (4=randInt) set result=date_" + "_int
	. else  if (5=randInt) set result=int_" + "_date
	else  if (6=randInt) do
	. ; TIMEWITHTIMEZONE + TIME
	. set result=$$getTimeTz_" + "_$$getTime
	else  if (7=randInt) do
	. ; TIME + TIMEWITHTIMEZONE
	. set result=$$getTime_" + "_$$getTimeTz
	else  if (8=randInt) do
	. ; TIME + TIMESTAMP
	. set result=$$getTime_" + "_$$getTimestamp
	else  if (9=randInt) do
	. ; TIMESTAMP + TIME
	. set result=$$getTimestamp_" + "_$$getTime
	else  if (10=randInt) do
	. ; TIME + TIMESTAMPWITHTIMEZONE
	. set result=$$getTime_" + "_$$getTimestampTz
	else  if (11=randInt) do
	. ; TIMESTAMPWITHTIMEZONE + TIME
	. set result=$$getTimestampTz_" + "_$$getTime
	else  if (12=randInt) do
	. ; NULL + TIME
	. set result=$$getNull_" + "_$$getTime
	else  if (13=randInt) do
	. ; TIME + NULL
	. set result=$$getTime_" + "_$$getNull
	else  if (14=randInt) do
	. ; NULL + TIMESTAMP
	. set result=$$getNull_" + "_$$getTimestamp
	else  if (15=randInt) do
	. ; TIMESTAMP + NULL
	. set result=$$getTimestamp_" + "_$$getNull
	quit result

getAddQueries()
	new queries
	set queries=100
	for queries=queries:-1:1 do
	. write "select "_$$getAddExpression_";",!
	quit

getSubExpression()
	new randInt,result
	set randInt=$random(9)
	; TODO (#1044): Remove the following line after time with time zone is properly supported
	set randInt=$select(randInt=4:6,randInt=7:6,1:randInt)
	if (0=randInt) do
	. ; DATE - DATE
	. set result=$$getDate_" - "_$$getDate
	else  if (1=randInt) do
	. ; DATE - TIME
	. set result=$$getDate_" - "_$$getTime
	else  if (2=randInt) do
	. ; DATE - INTEGER
	. set result=$$getDate_" - "_$$getInt
	else  if (3=randInt) do
	. ; DATE - NULL
	. set result=$$getDate_" - "_$$getNull
	else  if (4=randInt) do
	. ; TIMEWITHTIMEZONE - TIME
	. set result=$$getTimeTz_" - "_$$getTime
	else  if (5=randInt) do
	. ; TIMESTAMP - TIME
	. set result=$$getTimestamp_" - "_$$getTime
	else  if (6=randInt) do
	. ; TIMESTAMPWITHTIMEZONE - TIME
	. set result=$$getTimestampTz_" - "_$$getTime
	else  if (7=randInt) do
	. ; TIMEWITHTIMEZONE - NULL
	. set result=$$getTimeTz_" - "_$$getNull
	else  if (8=randInt) do
	. ; TIMESTAMP - NULL
	. set result=$$getTimestamp_" - "_$$getNull
	quit result

getSubQueries()
	new queries
	set queries=100
	; Get first operand
	new dt
	for queries=queries:-1:1 do
	. write "select "_$$getSubExpression_";",!
	quit

getCompOp()
	new randInt
	set randInt=$random(6) ;0-5
	quit $select(0=randInt:"=",1=randInt:"!=",2=randInt:"<",3=randInt:"<=",4=randInt:">",1:">=")

getCompExpression()
	new randInt,result
	; TODO (#1044): Include time with time zone cases after it is fully supported
	set randInt=$random(10)
	if (0=randInt) do
	. ; DATE DATE
	. set result=$$getDate_" "_$$getCompOp_" "_$$getDate
	else  if (1=randInt) do
	. ; DATE TIMESTAMP
	. set result=$$getDate_" "_$$getCompOp_" "_$$getTimestamp
	else  if (2=randInt) do
	. ; DATE TIMESTAMP WITH TIME ZONE
	. set result=$$getDate_" "_$$getCompOp_" "_$$getTimestampTz
	else  if (3=randInt) do
	. ; TIMESTAMP TIMESTAMP
	. set result=$$getTimestamp_" "_$$getCompOp_" "_$$getTimestamp
	else  if (4=randInt) do
	. ; TIMESTAMP TIMESTAMP WITH TIME ZONE
	. set result=$$getTimestamp_" "_$$getCompOp_" "_$$getTimestampTz
	else  if (5=randInt) do
	. ; TIMESTAMP DATE
	. set result=$$getTimestamp_" "_$$getCompOp_" "_$$getDate
	else  if (6=randInt) do
	. ; TIMESTAMP WITH TIME ZONE TIMESTAMP WITH TIME ZONE
	. set result=$$getTimestampTz_" "_$$getCompOp_" "_$$getTimestampTz
	else  if (7=randInt) do
	. ; TIMESTAMP WITH TIME ZONE TIMESTAMP
	. set result=$$getTimestampTz_" "_$$getCompOp_" "_$$getTimestamp
	else  if (8=randInt) do
	. ; TIMESTAMP WITH TIME ZONE DATE
	. set result=$$getTimestampTz_" "_$$getCompOp_" "_$$getDate
	else  if (9=randInt) do
	. ; TIME TIME
	. set result=$$getTime_" "_$$getCompOp_" "_$$getTime
	else  if (10=randInt) do
	. ; TIME TIME WITH TIME ZONE
	. set result=$$getTime_" "_$$getCompOp_" "_$$getTimeTz
	else  if (11=randInt) do
	. ; TIME WITH TIME ZONE TIME WITH TIME ZONE
	. set result=$$getTimeTz_" "_$$getCompOp_" "_$$getTimeTz
	else  if (12=randInt) do
	. ; TIME WITH TIME ZONE TIME
	. set result=$$getTimeTz_" "_$$getCompOp_" "_$$getTime
	quit result

getDorTs()
	new randInt
	set randInt=$random(3) ; 0-2
	; DATE or TIMESTAMP or TIMESTAMP WITH TIME ZONE
	quit $select(0=randInt:$$getDate,1=randInt:$$getTimestamp,1:$$getTimestampTz)

getT()
	; TODO (#1044) include time with time zone here
	quit $$getTime

getBetweenExpression()
	new randInt,result
	set randInt=$random(4) ; 0-3
	if (0=randInt) do
	. ; DATE BETWEEN DATE AND DATE
	. set result=$$getDorTs_" between "_$$getDorTs_" and "_$$getDorTs
	else  if (1=randInt) do
	. ; DATE NOT BETWEEN DATE AND DATE
	. set result=$$getDorTs_" not between "_$$getDorTs_" and "_$$getDorTs
	else  if (2=randInt) do
	. ; Time BETWEEN TIME AND TIME
	. set result=$$getT_" between "_$$getT_" and "_$$getT
	else  do
	. ; Time NOT BETWEEN TIME AND TIME
	. set result=$$getT_" not between "_$$getT_" and "_$$getT
	quit result

getInExpression()
	new randInt,result
	set randInt=$random(8) ; 0-7
	if (0=randInt) do
	. ; DATE IN DATE,DATE
	. set result=$$getDorTs_" in("_$$getDorTs_","_$$getDorTs_")"
	else  if (1=randInt) do
	. ; DATE NOT IN DATE,DATE
	. set result=$$getDorTs_" not in("_$$getDorTs_","_$$getDorTs_")"
	else  if (2=randInt) do
	. ; DATE IN (select DATE)
	. set result=$$getDorTs_" in(select "_$$getDorTs_")"
	else  if (3=randInt) do
	. ; DATE NOT IN (select DATE)
	. set result=$$getDorTs_" not in(select "_$$getDorTs_")"
	else  if (4=randInt) do
	. ; TIME IN TIME,TIME
	. set result=$$getT_" in("_$$getT_","_$$getT_")"
	else  if (5=randInt) do
	. ; TIME NOT IN TIME,TIME
	. set result=$$getT_" not in("_$$getT_","_$$getT_")"
	else  if (6=randInt) do
	. ; TIME IN (SELECT TIME)
	. set result=$$getT_" in(select "_$$getT_")"
	else  if (7=randInt) do
	. ; TIME NOT IN (SELECT TIME)
	. set result=$$getT_" not in(select "_$$getT_")"
	quit result

getCompQueries()
	new queries
	set queries=100
	; Get first operand
	for queries=queries:-1:1 do
	. if (80>queries) write "select "_$$getCompExpression_";",!
	. else  if (90>queries) write "select "_$$getBetweenExpression_";",!
	. else  write "select "_$$getInExpression_";",!
	quit

unionQueries(val1,val2)
	new val
	set val(0)=val1
	set val(0,1)=val2
	; Generate the following type of statements for the above values
	; (select time'01:01:01') union (select date'2023-01-01');
	; select date'2023-01-01') union (select time'01:01:01');
	; select date'2023-01-01') union (select date'2023-01-01') union (select time'01:01:01');
	; select time'01:01:01') union (select date'2023-01-01') union (select date'2023-01-01');
	; select date'2023-01-01') union (select date'2023-01-01' union (select time'01:01:01'));
	; select time'01:01:01') union (select date'2023-01-01' union (select date'2023-01-01'));
	write "(select "_val(0)_") union (select "_val(0,1)_");",!
	write "(select "_val(0,1)_") union (select "_val(0)_");",!
	write "(select "_val(0,1)_") union (select "_val(0,1)_") union (select "_val(0)_");",!
	write "(select "_val(0)_") union (select "_val(0,1)_") union (select "_val(0,1)_");",!
	write "(select "_val(0,1)_") union (select "_val(0,1)_" union (select "_val(0)_"));",!
	write "(select "_val(0)_") union (select "_val(0,1)_" union (select "_val(0,1)_"));",!
	quit

; Gets set operation with mismatching date/time types
tdtt089()
	new val
	; time, date
	set val(0)="time'01:01:01'"
	set val(0,1)="date'2023-01-01'"
	; timestamp with time zone, time
	set val(1)="timestamp with time zone'2023-01-01 01:01:01-05'"
	set val(1,1)="time'01:01:01'"
	; timestam with time zone, time with time zone
	set val(2)="timestamp with time zone'2023-01-01 01:01:01-05'"
	set val(2,1)="time with time zone'01:01:01'"
	; time with time zone, date
	set val(3)="time with time zone'01:01:01'"
	set val(3,1)="date'2023-01-01'"
	; timestamp, time
	set val(4)="timestamp'2023-01-01 01:01:01'"
	set val(4,1)="time'01:01:01'"
	; timestamp, time with time zone
	set val(5)="timestamp'2023-01-01 01:01:01'"
	set val(5,1)="time with time zone'01:01:01'"
	for i=0:1:5 do unionQueries(val(i),val(i,1))
	quit

; Gets set operation with text to date/time conversions
tdtt090()
	new val
	; string, date
	; string, time
	; string, time with time zone
	; string, timestamp
	; string, timestamp with time zone
	set val(6)="'text'"
	set val(6,0)="date'2023-01-01'"
	set val(6,1)="time'01:01:01'"
	set val(6,2)="timestamp'2023-01-01 01:01:01'"
	set val(6,3)="timestamp with time zone'2023-01-01 01:01:01'"
	set val(6,4)="time with time zone'01:01:01'"
	for i=0:1:4 do unionQueries(val(6),val(6,i))
	quit

; Return queries with arbitrary number of spaces between date and time of a timestamp, also towards the start and end of a literal
tdtt102()
	new randInt
	set randInt=$random(10)+1 ; 1-11
	set spaceStr=""
	for i=1:1:randInt set spaceStr=spaceStr_" "
	; date
	write "select date'"_spaceStr_"2023-01-01';",!
	write "select date'2023-01-01"_spaceStr_"';",!
	write "select date'2023-01-01"_spaceStr_"01:01:01';",!
	write "select date'2023-01-01 01:01:01"_spaceStr_"';",!
	write "select date'"_spaceStr_"2023-01-01 01:01:01';",!
	; timestamp
	write "select timestamp'"_spaceStr_"2023-01-01';",!
	write "select timestamp'2023-01-01 01:01:01"_spaceStr_"';",!
	write "select timestamp'2023-01-01"_spaceStr_"01:01:01';",!
	write "select timestamp'2023-01-01 01:01:01"_spaceStr_"';",!
	write "select timestamp'"_spaceStr_"2023-01-01 01:01:01';",!
	; timestamp with time zone
	write "select timestamp with time zone'"_spaceStr_"2023-01-01';",!
	write "select timestamp with time zone'2023-01-01 01:01:01-05:00"_spaceStr_"';",!
	write "select timestamp with time zone'2023-01-01"_spaceStr_"01:01:01-05:00';",!
	write "select timestamp with time zone'2023-01-01 01:01:01-05:00"_spaceStr_"';",!
	write "select timestamp with time zone'"_spaceStr_"2023-01-01 01:01:01-05:00';",!
	; time
	write "select time'"_spaceStr_"01:01:01';",!
	write "select time'01:01:01-05:00"_spaceStr_"';",!
	write "select time'01:01:01"_spaceStr_"';",!
	write "select time'"_spaceStr_"01:01:01-05:00';",!
	write "select time'"_spaceStr_"01:01:01';",!
	; timestamp and timestamp with timezone with arbitrary number of T's inbetween date and time
	set tStr=""
	for i=1:1:randInt set tStr=tStr_" "
	write "select timestamp'2023-01-01"_tStr_"01:01:01';",!
	write "select timestamp with time zone'2023-01-01"_tStr_"01:01:01-05:00';",!
	write "drop table if exists timestamptdtt102rw;",!
	write "create table timestamptdtt102rw (id int, dob timestamp);",!
	write "insert into timestamptdtt102rw values(1,timestamp'2023-01-01"_tStr_"01:01:01');",!
	write "select * from timestamptdtt102rw;",!
	write "drop table if exists timestamptztdtt102rw;",!
	write "create table timestamptztdtt102rw (id int, dob timestamp with time zone);",!
	write "insert into timestamptztdtt102rw values(1,timestamp with time zone'2023-01-01"_tStr_"01:01:01-05');",!
	write "select * from timestamptztdtt102rw;",!
	write "drop table timestamptdtt102rw;",!
	write "drop table timestamptztdtt102rw;",!
	set ^tdtt102timestamp(1)="2023-01-01"_tStr_"01:01:01"
	set ^tdtt102timestamptz(1)="2023-01-01"_tStr_"01:01:01-05"

	quit

tdtt104
	set subtest=$zcmdline
	zwr subtest
	set isPrimaryKey=$select(""=subtest:0,1:1)
	if (isPrimaryKey) do datetimemglobal(1) set inputStr="inputprim" write "hello",!
	else  do datetimemglobal set inputStr="input" write "hellowso",!
	set cnt=0
	; Add `"timetz"` after YDBOcto#1044 is fixed
	for tp="date","time","timestamp","timestamptz" do
	. for fmt="","fileman","horolog","zhorolog","zut" do
	. . set filename=inputStr_cnt_".sql"
	. . open filename:(append)
	. . use filename
	. . set tname=tp_fmt_"r"
	. . set dtcolumn="dob "
	. . ; Type
	. . set type=""
	. . if (tp["tz") do
	. . . set tp1=$select(tp["timestamp":"timestamp",1:"time") ; Time, Timestamp
	. . . set tp2="with time zone"
	. . . if (""=fmt) do
	. . . . set type=tp1_" "_tp2
	. . . . set dtcolumn=dtcolumn_type
	. . . else  do
	. . . . set type=tp1_"("_fmt_")"_tp2
	. . . . set dtcolumn=dtcolumn_type
	. . else  do
	. . . set type=tp_$select(""=fmt:"",1:"("_fmt_")")
	. . . set dtcolumn=dtcolumn_type
	. . write "drop table if exists "_tname_";",!
	. . if (isPrimaryKey) do
	. . . write "create table "_tname_" ("_dtcolumn_" primary key, "
	. . . write "id integer"
	. . else  do
	. . . write "create table "_tname_" (id integer primary key, "
	. . . write dtcolumn
	. . if (isPrimaryKey) set globalName="^datetimep"_tp_fmt
	. . else  set globalName="^datetime"_tp_fmt
	. . write ") global """_globalName_""" readonly;",!
	. . write "select * from "_tname_";",!
	. . if (isPrimaryKey) do
	. . . write "select * from "_tname_" where dob = "_type_"'"_map(tp,fmt,1)_"';",!
	. . else  do
	. . . write "select * from "_tname_" where dob = "_type_"'"_@globalName@(1)_"';",!
	. . write !
	. . close filename
	. . set filename=inputStr_cnt_".sh"
	. . open filename:(append)
	. . use filename
	. . write "yottadb -run %XCMD '"
	. . if (isPrimaryKey) do
	. . . write "kill "_globalName_"("""_map(tp,fmt,1)_""")"
        . . . write " set "_globalName_"("""_map(tp,fmt,1)_""")=3"
	. . else  do
	. . . write "kill "_globalName_"(1)"
	. . . write " set "_globalName_"(3)="""_@globalName@(1)_""""
	. . write "'",!
	. . close filename
	. . ;
	. . set filename=inputStr_cnt_".sql"
	. . ;
	. . set cnt=cnt+1
	quit
	;
unixepoch ; Create Tables to compare output at Unix Epoch
	; Timezone set by test to be UTC exactly. This test depends on that.
	set map("date","",-1)="1969-12-31"
	set map("date","",0)="1970-01-01"
	set map("date","",1)="1970-01-01" ; can't use
	set map("time","",-1)="23:59:59.999999"
	set map("time","",0)="00:00:00"
	set map("time","",1)="00:00:00.000001"
	set map("timetz","",-1)="23:59:59.999999-00:00"
	set map("timetz","",0)="00:00:00-00:00"
	set map("timetz","",1)="00:00:00.000001-00:00"
	set map("timestamp","",-1)="1969-12-31 23:59:59.999999"
	set map("timestamp","",0)="1970-01-01 00:00:00"
	set map("timestamp","",1)="1970-01-01 00:00:00.000001"
	set map("timestamptz","",-1)="1969-12-31 23:59:59.999999-00:00"
	set map("timestamptz","",0)="1970-01-01 00:00:00-00:00"
	set map("timestamptz","",1)="1970-01-01 00:00:00.000001-00:00"
	set map("date","fileman",-1)="2691231"
	set map("date","fileman",0)="2700101"
	set map("date","fileman",1)="2700101" ; can't use
	set map("time","fileman",-1)="235959"
	set map("time","fileman",0)=""
	set map("time","fileman",1)="" ; can't use
	set map("timetz","fileman",-1)="235959"
	set map("timetz","fileman",0)=""
	set map("timetz","fileman",1)="" ; can't use
	set map("timestamp","fileman",-1)="2691231.235959"
	set map("timestamp","fileman",0)="2700101"
	set map("timestamp","fileman",1)="2700101"
	set map("timestamptz","fileman",-1)="2691231.235959"
	set map("timestamptz","fileman",0)="2700101"
	set map("timestamptz","fileman",1)="2700101" ; can't use
	set map("date","horolog",-1)="47116"
	set map("date","horolog",0)="47117"
	set map("date","horolog",1)="47117"          ; can't use
	set map("time","horolog",-1)="86399"
	set map("time","horolog",0)="0"
	set map("time","horolog",1)="0"
	set map("timetz","horolog",-1)="86399"
	set map("timetz","horolog",0)="0"
	set map("timetz","horolog",1)="0"
	set map("timestamp","horolog",-1)="47116,86399"
	set map("timestamp","horolog",0)="47117,0"
	set map("timestamp","horolog",1)="47117,0"   ; We can't use that!
	set map("timestamptz","horolog",-1)="47116,86399"
	set map("timestamptz","horolog",0)="47117,0"
	set map("timestamptz","horolog",1)="47117,0" ; We can't use that!
	set map("date","zhorolog",-1)="47116,,,"
	set map("date","zhorolog",0)="47117,,,"
	set map("date","zhorolog",1)="47117,0,1,"
	set map("time","zhorolog",-1)="47116,86399,999999,"
	set map("time","zhorolog",0)=",0,,"
	set map("time","zhorolog",1)=",0,1,"
	set map("timetz","zhorolog",-1)=",86399,999999,0"
	set map("timetz","zhorolog",0)=",0,,0"
	set map("timetz","zhorolog",1)=",0,1,0"
	set map("timestamp","zhorolog",-1)="47116,86399,999999,0"
	set map("timestamp","zhorolog",0)="47117,0,0,0"
	set map("timestamp","zhorolog",1)="47117,0,1,0"
	set map("timestamptz","zhorolog",-1)="47116,86399,999999,0"
	set map("timestamptz","zhorolog",0)="47117,,,0"
	set map("timestamptz","zhorolog",1)="47117,,,0"
	set map("date","zut",-1)="-1"
	set map("date","zut",0)="0"
	set map("date","zut",1)="1"
	set map("time","zut",-1)="-1"
	set map("time","zut",0)="0"
	set map("time","zut",1)="1"
	set map("timetz","zut",-1)="-1"
	set map("timetz","zut",0)="0"
	set map("timetz","zut",1)="1"
	set map("timestamp","zut",-1)="-1"
	set map("timestamp","zut",0)="0"
	set map("timestamp","zut",1)="1"
	set map("timestamptz","zut",-1)="-1"
	set map("timestamptz","zut",0)="0"
	set map("timestamptz","zut",1)="1"
	do epochSharedCode
	;
	quit
	;
regularDate ; Create tables for regular dates of 2020-02-14 05:14:32.009922 and 1947-03-10 16:13:22.582
	set map("date","",0)="2020-02-14"
	set map("date","",1)="1947-03-10"
	set map("time","",0)="05:14:32.009922"
	set map("time","",1)="16:13:22.582"
	set map("timetz","",0)="05:14:32.009922-00:00"
	set map("timetz","",1)="16:13:22.582-00:00"
	set map("timestamp","",0)="2020-02-14 05:14:32.009922"
	set map("timestamp","",1)="1947-03-10 16:13:22.582"
	set map("timestamptz","",0)="2020-02-14 05:14:32.009922-00:00"
	set map("timestamptz","",1)="1947-03-10 16:13:22.582-00:00"
	set map("date","fileman",0)="3200214"
	set map("date","fileman",1)="2470310"
	set map("time","fileman",0)="051432"
	set map("time","fileman",1)="161322"
	set map("timetz","fileman",0)="051432-00:00"
	set map("timetz","fileman",1)="161322-00:00"
	set map("timestamp","fileman",0)="3200214.051432"
	set map("timestamp","fileman",1)="2470310.161322"
	set map("timestamptz","fileman",0)="3200214.051432-00:00"
	set map("timestamptz","fileman",1)="2470310.161322-00:00"
	set map("date","horolog",0)="65423"
	set map("date","horolog",1)="38784"
	set map("time","horolog",0)="18872"
	set map("time","horolog",1)="58402"
	set map("timetz","horolog",0)="18872"
	set map("timetz","horolog",1)="58402"
	set map("timestamp","horolog",0)="65423,18872"
	set map("timestamp","horolog",1)="38784,58402"
	set map("timestamptz","horolog",0)="65423,18872"
	set map("timestamptz","horolog",1)="38784,58402"
	set map("date","zhorolog",0)="65423,,,"
	set map("date","zhorolog",1)="38784,,,"
	set map("time","zhorolog",0)=",18872,009922,"
	set map("time","zhorolog",1)=",58402,582000,"
	set map("timetz","zhorolog",0)=",18872,009922,"
	set map("timetz","zhorolog",1)=",58402,582000,"
	set map("timestamp","zhorolog",0)="65423,18872,009922,0"
	set map("timestamp","zhorolog",1)="38784,58402,582000,0"
	set map("timestamptz","zhorolog",0)="65423,18872,009922,0"
	set map("timestamptz","zhorolog",1)="38784,58402,582000,0"
	set map("date","zut",0)="1581657272009922"
	set map("date","zut",1)="-719912797418000"
	set map("time","zut",0)="1581657272009922"
	set map("time","zut",1)="-719912797418000"
	set map("timetz","zut",0)="1581657272009922"
	set map("timetz","zut",1)="-719912797418000"
	set map("timestamp","zut",0)="1581657272009922"
	set map("timestamp","zut",1)="-719912797418000"
	set map("timestamptz","zut",0)="1581657272009922"
	set map("timestamptz","zut",1)="-719912797418000"
	do epochSharedCode
	quit
	;
epochSharedCode ; Shared code for beforeepoch afterepoch and unixepoch
	write "--;",!
	write "--Test start;",!
	write "--;",!
	new tablenames,basetype
	for type="date","time","timetz","timestamp","timestamptz" do
	. for format="","fileman","horolog","zhorolog","zut" do
	. . ; invalid formats
	. . if type="time",format="fileman" quit
	. . if type="time",format="zut" quit
	. . if type="timetz",format="fileman" quit
	. . if type="timetz",format="zut" quit
	. . if type="timestamptz",format="zut" quit
	. . ;
	. . ; Set base type
	. . if (type="time")!(type="timetz") set basetype="TIME"
	. . if type="date" set basetype="DATE"
	. . if (type="timestamp")!(type="timestamptz") set basetype="TIMESTAMP"
	. . ;
	. . set global="^"_type_format
	. . kill @global
	. . for i=-1:1:1 if $data(map(type,format,i)) set @global@(i)=map(type,format,i)
	. . new tablename set tablename=type_format
	. . new dateformat,hastimezone
	. . set hastimezone=0
	. . if type="timetz"      set hastimezone=1,type="time"
	. . if type="timestamptz" set hastimezone=1,type="timestamp"
	. . set dateformat=$zconvert(type,"u")
	. . if format'="" set dateformat=dateformat_"("_$zconvert(format,"u")_")"
	. . if hastimezone set dateformat=dateformat_" with time zone"
	. . write "-- date format "_dateformat_";",!
	. . write "DROP TABLE IF EXISTS "_tablename_" KEEPDATA;",!
	. . write "CREATE TABLE "_tablename_" (id INTEGER PRIMARY KEY, dob "_dateformat_") GLOBAL """_global_""" READONLY;",!
	. . write "SELECT * FROM "_tablename_";",!
	. . write "--;",!
	. . ;
	. . set tablenames(tablename)=basetype  ; Store the type along with the table name
    ;
	write "--;",!
	write "--Joins;",!
	write "--;",!
	write "--Only compatible type joins are included;",!
    ;
	; Helper function to check if types are compatible
	new isinCompatible
	set isinCompatible("DATE","TIME")=""
	set isinCompatible("TIMESTAMP","TIME")=""
	;
	; Now for the inner joins
	new tablename1 set tablename1=""
	new type1,type2
	for  set tablename1=$order(tablenames(tablename1)) quit:tablename1=""  do
	. new tablename2 set tablename2=""
	. for  set tablename2=$order(tablenames(tablename2)) quit:tablename2=""  do
	. . if tablename1=tablename2 quit  ; same table
	. . set type1=tablenames(tablename1)
	. . set type2=tablenames(tablename2)
	. . if $data(isinCompatible(type1,type2)) quit  ; incompatible types
	. . if $data(isinCompatible(type2,type1)) quit  ; incompatible types
	. . write "-- join between "_tablename1_" and "_tablename2_";",!
	. . write "SELECT * FROM "_tablename1_" t1 INNER JOIN "_tablename2_" t2 ON t1.dob = t2.dob;",!
	quit