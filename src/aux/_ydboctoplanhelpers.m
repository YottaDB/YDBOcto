;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Below helper functions are used by the generated Octo Plans (M files _ydboctoP*.m)
; -------------------------------------------------------------

%ydboctoplanhelpers	;
	QUIT

initDateTimeTypes
	SET date=17			; Refers SqlValueType.DATE_LITERAL
	SET time=18			; Refers SqlValueType.TIME_LITERAL
	SET timeWithTimeZone=19		; Refers SqlValueType.TIME_WITH_TIME_ZONE_LITERAL
	SET timestamp=20		; Refers SqlValueType.TIMESTAMP_LITERAL
	SET timestampWithTimeZone=21	; Refers SqlValueType.TIMESTAMP_WITH_TIME_ZONE_LITERAL
	SET dateType=6			; Refers to SqlDataType.DATE_TYPE
	SET timeType=7			; Refers to SqlDataType.TIME_TYPE
	SET timeWithTimeZoneType=8	; Refers to SqlDataType.TIME_WITH_TIME_ZONE_TYPE
	SET timestampType=9		; Refers to SqlDataType.TIMESTAMP_TYPE
	SET timestampWithTimeZoneType=10; Refers to SqlDataType.TIMESTAMP_WITH_TIME_ZONE_TYPE
	SET horologFormat=32			; Refers to OptionalKeyword.OPTIONAL_DATE_TIME_HOROLOG
	SET zhorologFormat=33			; Refers to OptionalKeyword.OPTIONAL_DATE_TIME_ZHOROLOG
	SET filemanFormat=34			; Refers to OptionalKeyword.OPTIONAL_DATE_TIME_FILEMAN
	SET zutFormat=35			; Refers to OptionalKeyword.OPTIONAL_DATE_TIME_ZUT
	SET textFormat=36			; Refers to OptionalKeyword.OPTIONAL_DATE_TIME_TEXT
	QUIT

; Convert the given unix time value to local time zone value
ConvertToLocalTimezone(type,op1)
	QUIT:($ZYISSQLNULL(op1)) $ZYSQLNULL
	QUIT $&octo.ydboctoConvertToLocalTimezoneM(type,op1)

; For input and output details refer to called external function.
; This function is responsible for only checking and return ZYSQLNULL if any operand is ZYSQLNULL
SubDate(op1,op2,isInt)
	NEW result
	QUIT:($ZYISSQLNULL(op1)!$ZYISSQLNULL(op2)) $ZYSQLNULL
	SET result=$&octo.ydboctoSubDateM(op1,op2,isInt)
	IF (253402300799999999+1)=result DO ; DATE_TIME_ERROR_RETURN
	. ; ERROR, result exceeding date/time range
	. ZMESSAGE %ydboctoerror("DATETIMERESULTOUTOFRANGE")
	QUIT result

; For input and output details refer to called external function.
; This function is responsible for only checking and return ZYSQLNULL if any operand is ZYSQLNULL
SubDateTime(op1,op1type,op2,op2type)
	new result
	QUIT:($ZYISSQLNULL(op1)!$ZYISSQLNULL(op2)) $ZYSQLNULL
	set result=$&octo.ydboctoSubDateTimeM(op1,op1type,op2,op2type)
	IF (253402300799999999+1)=result DO ; DATE_TIME_ERROR_RETURN
	. ; ERROR, result exceeding date/time range
	. ZMESSAGE %ydboctoerror("DATETIMERESULTOUTOFRANGE")
	QUIT result


; For input and output details refer to ydboctoAddDateTimeM documentation.
; This function is responsible for only checking and return ZYSQLNULL if any operand is ZYSQLNULL
AddDateTime(op1,op1type,op2,op2type)
	new result
	QUIT:($ZYISSQLNULL(op1)!$ZYISSQLNULL(op2)) $ZYSQLNULL
	set result=$&octo.ydboctoAddDateTimeM(op1,op1type,op2,op2type)
	IF (253402300799999999+1)=result DO ; DATE_TIME_ERROR_RETURN
	. ; ERROR, result exceeding date/time range
	. ZMESSAGE %ydboctoerror("DATETIMERESULTOUTOFRANGE")
	QUIT result

; Input
;	value - string to be casted
; 	valueFormat - format in which the input is expected to be
;	coerceType - result type expected
; Output
;	Success: returns casted value in internal format or ZYSQLNULL if input is ZYSQLNULL and ""
; 	Error:	 INVALIDDATETIMECONVERSION error is issued
String2DateTimeCast(value,valueFormat,coerceType)
	QUIT:$ZYISSQLNULL(value) $ZYSQLNULL
	QUIT:""=value $ZYSQLNULL
	NEW result,type
	DO initDateTimeTypes
	if (dateType=coerceType) set type=date
	else  if (timeType=coerceType) set type=time
	else  if (timeWithTimeZoneType=coerceType) set type=timeWithTimeZone
	else  if (timestampType=coerceType) set type=timestamp
	else  if (timestampWithTimeZoneType=coerceType) set type=timestampWithTimeZone
	; value, type, format_str, text_format_specifier
	SET result=$&octo.ydboctoValidateDateTimeValueM(value,type,textFormat,valueFormat)
	IF (result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=value  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. ; At present we only support text formatted input
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=textFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	SET result=$&octo.ydboctoDateTimeStringCastM(value,valueFormat,coerceType)
	QUIT result

; Following routine converts the `value` given from `preCoerceType` to `coerceType`
; Input
;	value - date/time value in internal format to be converted
; 	preCoerceType - type of the operand
;	coerceType - type to be casted to
; Output
;	returns casted value in internal format	or ZYSQLNULL if input is ZYSQLNULL and ""
DateTimeCast(value,preCoerceType,coerceType)
	QUIT:$ZYISSQLNULL(value) $ZYSQLNULL
	QUIT:""=value $ZYSQLNULL
	QUIT $&octo.ydboctoDateTimeCastM(value,preCoerceType,coerceType)

; Input
; 	value - date/time value in internal format
;	columnType - date/time column type
;	outputFormat - expected output format of the result (horolog,text,zhorolog,..)
;	textFormatSpecifier - format in which the result has to be if the outputFormat is `text`
; Output
;	date/time value in output format specified
;	ZYSQLNULL if input is ZYSQLNULL or ""
; Note: This routine also handled array value input by removing and placing back `{}`
PrintDateTimeResultColumnValue(value,columnType,outputFormat,textFormatSpecifier)
	QUIT:$ZYISSQLNULL(value) $ZYSQLNULL
	QUIT:""=value $ZYSQLNULL
	NEW result,format
	DO initDateTimeTypes
	; Check if the value is in the form of an array
	NEW isArray
	IF (value["{") SET isArray=1
	ELSE  SET isArray=0
	IF isArray do
	. SET value=$EXTRACT(value,2,$ZLENGTH(value)-1)

	IF (textFormat=outputFormat) DO
	. ; Converts the following type of column to Text based on format specifier given
	. ; 	date
	. ;	time
	. ;	timestamp
	. ;	time with time zone
	. ;	timestamp with time zone
	. NEW hasTimeZone SET hasTimeZone=0
	. ; Based on type add or remove format specifier for time zone
	. SET:(timestampWithTimeZone=columnType)!(timeWithTimeZone=columnType) hasTimeZone=1
	. ; Ensure 0 padding flag is added to Year
	. NEW tmpTextFormatStr
	. IF (date=columnType)!(timestamp=columnType)!(timestampWithTimeZone=columnType) DO
	. . SET tmpTextFormatStr=$PIECE(textFormatSpecifier,"Y",1)
	. . SET tmpTextFormatStr=tmpTextFormatStr_"4"
	. . SET tmpTextFormatStr=tmpTextFormatStr_"Y"_$PIECE(textFormatSpecifier,"Y",2)
	. ELSE  SET tmpTextFormatStr=textFormatSpecifier
	. SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,tmpTextFormatStr)
	ELSE  IF (horologFormat=outputFormat) DO
	. ; Note that the date and time format specifier passed in this block is driven by what is expected by the
	. ; date and time utility routines used to get the HOROLOG result
	. IF (date=columnType) DO
	. . ; Internal format to format expected by $$CDN^%H
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,"%m/%d/%Y")
	. . ; Text date to HOROLOG date
	. . SET result=$$CDN^%H(result)
	. ELSE  IF (time=columnType)!(timeWithTimeZone=columnType) DO
	. . ; Internal format to format expected by $$CTN^%H
	. . SET format="%H:%M:%S"
	. . SET:(timeWithTimeZone=columnType) format=format_"%z"
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,format)
	. . SET result=$PIECE(result,".",1) ; Remove microseconds if it exists, this also removes timezone
	. . ; Text time to HOROLOG time
	. . SET result=$$CTN^%H(result)
	. ELSE  IF (timestamp=columnType)!(timestampWithTimeZone=columnType) DO
	. . ; Internal format to format required by $$CDN^%H and $$CTN^%H
	. . SET format="%m/%d/%Y %H:%M:%S"
	. . SET:(timestampWithTimeZone=columnType) format=format_"%z"
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,format)
	. . ; Get date part
	. . NEW dateResult SET dateResult=$PIECE(result," ",1)
	. . ; Text date to HOROLOG date
	. . SET dateResult=$$CDN^%H(dateResult)
	. . ; Get time part
	. . NEW timeResult SET timeResult=$PIECE(result," ",2)
	. . SET timeResult=$PIECE(timeResult,".",1) ; Remove microseconds if it exists, this also removes timezone
	. . ; Text time to HOROLOG time
	. . SET timeResult=$$CTN^%H(timeResult)
	. . ; Form the timestamp which consists of both date and time
	. . SET result=dateResult_","_timeResult
	ELSE  IF (zhorologFormat=outputFormat) DO
	. ; zhorolog
	. ; 	date
	. ;	time
	. ;	timestamp
	. ;	time with time zone
	. ;	timestamp with time zone
	. ; Note that the date and time format specifier passed in this block is driven by what is expected by the
	. ; date and time utility routines used to get the HOROLOG result.
	. NEW timeZone SET timeZone=""
	. IF (date=columnType) DO
	. . ; Internal format to format expected by $$CDN^%H
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,"%m/%d/%Y")
	. . ; Text date to HOROLOG date
	. . SET result=$$CDN^%H(result)
	. . SET result=result_",,,"
	. ELSE  IF (time=columnType)!(timeWithTimeZone=columnType) DO
	. . ; Internal format to format expected by $$CTN^%H
	. . NEW format SET format="%H:%M:%S"
	. . SET:(timeWithTimeZone=columnType) format=format_"%z"
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,format)
	. . ; Text time to HOROLOG time
	. . NEW sign
	. . SET sign=$select((result["+"):"+",(result["-"):"-",1:"")
	. . SET:sign'="" timeZone=$PIECE(result,sign,2)
	. . SET:sign'="" result=$PIECE(result,sign,1)
	. . NEW timeH,timeM,timeS SET (timeH,timeM,timeS)=0
	. . NEW len SET len=$length(timeZone)
	. . IF (len=2) SET timeH=+timeZone
	. . ELSE  IF (len=5) DO
	. . . SET timeH=$EXTRACT(timeZone,1,len-3)
	. . . ; skip 3 as it is a :
	. . . SET timeM=$EXTRACT(timeZone,4,$length(timeZone))
	. . ELSE  DO
	. . . SET timeH=$EXTRACT(timeZone,1,len-6)
	. . . ; skip 3 as it is a :
	. . . SET timeM=$EXTRACT(timeZone,4,len-3)
	. . . ; skip 6 as it is a :
	. . . SET timeS=$EXTRACT(timeZone,7,len)
	. . SET timeH=timeH*60*60
	. . SET timeM=timeM*60
	. . SET timeZone=timeH+timeM+timeS
	. . SET:(timestampWithTimeZone=columnType) timeZone=$select(("+"=sign):"-",1:"")_timeZone
	. . NEW microsec
	. . SET microsec=$PIECE(result,".",2)
	. . SET result=$PIECE(result,".",1)
	. . SET result=$$CTN^%H(result)
	. . SET result=","_result_","_microsec_","
	. . SET:(timeWithTimeZone=columnType) result=result_timeZone
	. ELSE  IF (timestamp=columnType)!(timestampWithTimeZone=columnType) DO
	. . ; Internal format to format required by $$CDN^%H and $$CTN^%H
	. . NEW format SET format="%m/%d/%Y %H:%M:%S"
	. . SET:(timestampWithTimeZone=columnType) format=format_"%z"
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,format)
	. . ; Get date part
	. . NEW dateResult SET dateResult=$PIECE(result," ",1)
	. . ; Text date to HOROLOG date
	. . SET dateResult=$$CDN^%H(dateResult)
	. . ; Get time part
	. . NEW timeResult SET timeResult=$PIECE(result," ",2)
	. . NEW sign
	. . SET sign=$select((timeResult["+"):"+",(timeResult["-"):"-",1:"")
	. . SET:sign'="" timeZone=$PIECE(timeResult,sign,2)
	. . SET:sign'="" timeResult=$PIECE(timeResult,sign,1)
	. . NEW timeH,timeM,timeS SET (timeH,timeM,timeS)=0
	. . NEW len SET len=$length(timeZone)
	. . IF (len=2) SET timeH=+timeZone
	. . ELSE  IF (len=5) DO
	. . . SET timeH=$EXTRACT(timeZone,1,len-3)
	. . . ; skip 3 as it is a :
	. . . SET timeM=$EXTRACT(timeZone,4,$length(timeZone))
	. . ELSE  DO
	. . . SET timeH=$EXTRACT(timeZone,1,len-6)
	. . . ; skip 3 as it is a :
	. . . SET timeM=$EXTRACT(timeZone,4,len-3)
	. . . ; skip 6 as it is a :
	. . . SET timeS=$EXTRACT(timeZone,7,len)
	. . SET timeH=timeH*60*60
	. . SET timeM=timeM*60
	. . SET timeZone=timeH+timeM+timeS
	. . SET:(timestampWithTimeZone=columnType) timeZone=$select(("+"=sign):"-",1:"")_timeZone
	. . NEW microsec
	. . SET microsec=$PIECE(timeResult,".",2)
	. . SET timeResult=$PIECE(timeResult,".",1)
	. . ; Text time to HOROLOG time
	. . SET timeResult=$$CTN^%H(timeResult)
	. . ; Form the timestamp which consists of both date and time
	. . SET result=dateResult_","_timeResult_","_microsec_","
	. . SET:(timestampWithTimeZone=columnType) result=result_timeZone
	. ; Following SET ensures that values where time zone exceeds zhorolog format range is seen as empty string
	. SET:((timeWithTimeZone=columnType)!(timestampWithTimeZone=columnType))&((43200<timeZone)!(-50400>timeZone)) result=""
	ELSE  IF (zutFormat=outputFormat) DO
	. ; zut
	. ; 	date
	. ;	timestamp
	. ;	timestamp with time zone
	. ;
	. IF (time=columnType)!(timeWithTimeZone=columnType)!(timestampWithTimeZone=columnType) DO
	. . ; Text time to zut time
	. . SET result=""
	. ELSE  DO
	. . ; Extract last 6 digits which represent microseconds
	. . NEW microsec
	. . SET microsec=$EXTRACT(value,$length(value)-5,$length(value))
	. . NEW sec SET sec=$EXTRACT(value,1,$length(value)-6)
	. . ; Multiply seconds part by 1000000 to get microseconds and add the extracted part to this value
	. . SET sec=sec*1000000
	. . SET sec=sec+microsec
	. . SET result=sec
	ELSE  IF (filemanFormat=outputFormat) DO
	. IF (date=columnType) DO
	. . ; Internal format to format that horolog uses (just to make things uniform)
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,"%m/%d/%Y")
	. . ; Text date to Fileman date
	. . SET result=$$Text2FilemanDate(result)
	. ELSE  IF (time=columnType)!(timeWithTimeZone=columnType) DO
	. . SET result=""
	. ELSE  IF (timestamp=columnType)!(timestampWithTimeZone=columnType) DO
	. . ; Internal format to format that horolog uses (just to make things uniform)
	. . SET format="%m/%d/%Y %H:%M:%S"
	. . SET:(timestampWithTimeZone=columnType) format=format_"%z"
	. . SET result=$&octo.ydboctoDateTimeInternalFormat2TextM(value,columnType,format)
	. . ; Get date part
	. . NEW dateResult SET dateResult=$PIECE(result," ",1)
	. . ; Text date to Fileman date
	. . SET dateResult=$$Text2FilemanDate(dateResult)
	. . ; Get time part
	. . NEW timeResult SET timeResult=$PIECE(result," ",2)
	. . SET timeResult=$PIECE(timeResult,".",1) ; Remove microseconds and time zone if it exists
	. . SET timeResult=$PIECE(timeResult,"+",1) ; Remove time zone if it exists
	. . SET timeResult=$PIECE(timeResult,"-",1) ; Remove time zone if it exists
	. . ; Text time to Fileman time
	. . SET timeResult=$$Text2FilemanTime(timeResult)
	. . ; Form the timestamp which consists of both date and time
	. . SET result=dateResult_"."_timeResult
	;
	IF (""'=result) DO
	. SET res=$&octo.ydboctoValidateDateTimeValueM(result,columnType,outputFormat,textFormatSpecifier)
	. IF (res) DO
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=result  ; pass parameter to `src/ydb_error_check.c`
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=columnType
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=outputFormat
	. . ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF isArray SET result="{"_result_"}"
	QUIT result

Text2FilemanDate(inputStr)
	; $ZYSQLNULL is not expected here as `PrintDateTimeResultColumnValue` handles it before this call
	; format "%m/%d/%Y"
	NEW month,date,year,result
	SET month=$PIECE(inputStr,"/",1)
	SET date=$PIECE(inputStr,"/",2)
	SET year=$PIECE(inputStr,"/",3)
	; Convert year to fileman year
	SET year=+year-1700
	IF (3>$length(year)) DO
	. NEW i,length
	. SET length=$length(year)
	. SET length=3-length
	. FOR i=1:1:length DO
	. . SET year="0"_year
	SET result=year_month_date
	QUIT result

Text2FilemanTime(inputStr)
	; $ZYSQLNULL is not expected here as `PrintDateTimeResultColumnValue` handles it before this call
	; format "%H:%M:%S"
	NEW hour,minute,second,result
	SET hour=$PIECE(inputStr,":",1)
	SET minute=$PIECE(inputStr,":",2)
	SET second=$PIECE(inputStr,":",3)
	; Convert to fileman time
	SET result=hour_minute_second
	QUIT result

Text2UnixTime(inputStr,type,format)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	QUIT:""=inputStr $ZYSQLNULL
	NEW result
	DO initDateTimeTypes
	; Trim the value as this makes further processing difficult
	SET inputStr=$$FUNC^%TRIM(inputStr)
	IF (date=type)  ; no modifications here
	ELSE  IF (time=type) DO
	. SET:(format["%z") format=$PIECE(format,"%z")
	ELSE  IF (timeWithTimeZone=type) DO
	. SET:(format'["%z") format=format_"%z"
	. ; Add additional 00 to match %z
	. NEW timezone SET timezone=""
	. IF (inputStr["+") SET timezone=$piece(inputStr,"+",2)
	. ELSE  IF (inputStr["-") SET timezone=$piece(inputStr,"-",2)
	. IF (2=$length(timezone)) DO
	. . SET inputStr=inputStr_":00"
	ELSE  IF (timestamp=type) DO
	. SET:(format["%z") format=$PIECE(format,"%z")
	ELSE  IF (timestampWithTimeZone=type) DO
	. SET:(format'["%z") format=format_"%z"
	. ; Add additional 00 to match %z
	. NEW timezone SET timezone=""
	. IF (inputStr["+") SET timezone=$piece(inputStr,"+",2)
	. ELSE  IF (inputStr[":") DO
	. . ; We check for `:` because `-` can match date's delimiter too
	. . SET timezone=$extract(inputStr,$find(inputStr,":"),$length(inputStr))
	. . SET timezone=$piece(timezone,"-",2)
	. IF (2=$length(timezone)) DO
	. . SET inputStr=inputStr_":00"
	SET result=$&octo.ydboctoValidateDateTimeValueM(inputStr,type,textFormat,format)
	IF (result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=textFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF (date=type)!(time=type)!(timeWithTimeZone=type)!(timestamp=type)!(timestampWithTimeZone=type) DO
	. SET result=$&octo.ydboctoText2InternalFormatM(inputStr,format)
	QUIT result

Horolog2UnixTime(inputStr,type)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	QUIT:""=inputStr $ZYSQLNULL
	NEW result
	DO initDateTimeTypes
	SET result=$&octo.ydboctoValidateDateTimeValueM(inputStr,type,horologFormat)
	IF (result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=horologFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	SET inputStr=$$Horolog2Text(inputStr,type)
	IF (date=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y")
	ELSE  IF (time=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%H:%M:%S")
	ELSE  IF (timeWithTimeZone=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%H:%M:%S%z")
	ELSE  IF (timestamp=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y %H:%M:%S")
	ELSE  IF (timestampWithTimeZone=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y %H:%M:%S%z")
	QUIT result

Fileman2UnixTime(inputStr,type)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	QUIT:""=inputStr $ZYSQLNULL
	NEW result
	DO initDateTimeTypes
	SET result=$&octo.ydboctoValidateDateTimeValueM(inputStr,type,filemanFormat)
	IF (1=result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	ELSE  IF (2=result) QUIT $ZYSQLNULL ; This is the case where a column value is being processed,
				            ; just return ZYSQLNULL.
	SET inputStr=$$Fileman2Text(inputStr,type)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	IF (date=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y")
	ELSE  IF (timestamp=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y %H:%M:%S")
	ELSE  IF (timestampWithTimeZone=type) SET result=$&octo.ydboctoText2InternalFormatM(inputStr,"%m/%d/%Y %H:%M:%S%z")
	QUIT result

ZHorolog2UnixTime(inputStr,type)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	QUIT:""=inputStr $ZYSQLNULL
	NEW result
	DO initDateTimeTypes
	SET result=$&octo.ydboctoValidateDateTimeValueM(inputStr,type,zhorologFormat)
	IF (result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=zhorologFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	SET inputStr=$$ZHorolog2Text(inputStr,type)
	IF (date=type) DO
	. SET format="%m/%d/%Y"
	ELSE  IF (time=type) DO
	. SET format="%H:%M:%S"
	ELSE  IF (timeWithTimeZone=type) DO
	. SET format="%H:%M:%S%z"
	ELSE  IF (timestamp=type) DO
	. SET format="%m/%d/%Y %H:%M:%S"
	ELSE  IF (timestampWithTimeZone=type) DO
	. SET format="%m/%d/%Y %H:%M:%S%z"
	SET result=$&octo.ydboctoText2InternalFormatM(inputStr,format)
	QUIT result

ZUT2UnixTime(inputStr,type)
	QUIT:$ZYISSQLNULL(inputStr) $ZYSQLNULL
	QUIT:""=inputStr $ZYSQLNULL
	; validate input
	NEW result
	DO initDateTimeTypes
	SET result=$&octo.ydboctoValidateDateTimeValueM(inputStr,type,zutFormat)
	IF (result) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=zutFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	; Extract seconds and microseconds. Following code determines whether input is +ve or -ve then based on the number of
	; digits present seconds and micro seconds are extracted.
	NEW input SET input=inputStr
	NEW seconds
	NEW microseconds
	NEW isNeg
	IF (0>input) DO
	. ; -ve
	. SET isNeg=1
	ELSE  DO
	. ; +ve
	. SET isNeg=0
	IF (7<$length(input)) DO
	. ; both seconds and microseconds exist
	. SET seconds=$EXTRACT(input,1,$length(input)-6)
	. SET seconds=+seconds
	. SET microseconds=$EXTRACT(input,$length(input)-5,$length(input))
	ELSE  DO
	. ; only microseconds exist
	. SET seconds=0
	. SET microseconds=input
	. SET microseconds=+microseconds
	. SET:(1=isNeg) microseconds=-microseconds
	; adjust microsecond value
	NEW len SET len=$length(microseconds)
	NEW mult SET mult=6-len
	FOR i=1:1:mult SET microseconds="0"_microseconds
	IF (isNeg) SET microseconds="-"_microseconds
	SET microseconds=+microseconds
	; decrement second if microsecond value is negative
	IF (0>microseconds) DO
	. SET seconds=seconds-1
	. SET microseconds=1000000+microseconds
	; Convert to internal format, we do not expect time types here
	IF (date=type) SET result=$&octo.ydboctoZutM(seconds,microseconds,"%m/%d/%Y")
	ELSE  IF (timestamp=type) SET result=$&octo.ydboctoZutM(seconds,microseconds,"%m/%d/%Y %H:%M:%S")
	QUIT result

; type can be "date","time","timestamp"
; "date" means inputStr has 1 value whose value represents date
; "time","timeWithTimeZone" means inputstr has 1 value whose value represents time
; "timestamp","timestampWithTimeZone" means inputStr has 2 values `date,time` with a comma as delimiter, they represent timestamp
Horolog2Text(inputStr,type)
	; Convert $HOROLOG format date/time/date&time to the same in text format
	NEW result
	IF (date=type) DO
	. ; Convert "date" type HOROLOG value to text format
	. SET result=$ZDATE(inputStr,"MM/DD/YEAR")
	ELSE  IF (time=type)!(timeWithTimeZone=type) DO
	. ; Convert "time" type HOROLOG value to text format
	. ; Note $ZDATE expects ",3661" syntax if only time value is given
	. SET result=$ZDATE(","_inputStr,"24:60:SS")
	ELSE  IF (timestamp=type)!(timestampWithTimeZone=type) DO
	. ; Convert "timestamp" type HOROLOG value to text format
	. SET result=$ZDATE(inputStr,"MM/DD/YEAR 24:60:SS")
	ELSE  SET result=""
	QUIT result

ZHorolog2Text(inputStr,type)
	; Convert $ZHOROLOG format date/time/date&time with/without timezone to the same in text format
	; Convert "date" type ZHOROLOG value to text format
	; Handle "time" type ZHOROLOG value to text format
	; Handle "timestamp" type ZHOROLOG value to text format
	NEW result
	; IF ("date"=type) format="%m/%d/%Y"
	; IF ("time"=type) format="%H:%M:%S"
	; IF ("timeWithTimeZone"=type) format="%H:%M:%S%z"
	; IF ("timestamp"=type) format="%m/%d/%Y %H:%M:%S"
	; IF ("timestampWithTimeZone"=type) format="%m/%d/%Y %H:%M:%S%z"
	; Split the values
	NEW microsec,timezone,timezoneH,timezoneM,timezoneS,sign
	SET microsec=$PIECE(inputStr,",",3)
	SET timezone=$PIECE(inputStr,",",4)
	SET microsec=+microsec ; It is possible that + sign is present in this value get rid of it
	IF (""=timezone) DO
	. ; There is no timezone information present
	ELSE  DO
	. SET sign=$select(0>timezone:-1,1:1)
	. SET timezone=sign*timezone ; negative number to positive number
	. ; timezone value is in seconds
	. SET timezoneS=(timezone#3600)#60 ; `#3600` removes hours then `#60` removes minutes, remaining is timezone seconds
	. SET timezoneH=(timezone/3600)\1 ; -> hours as an integer
	. SET timezoneM=((timezone#3600)/60)\1 ;-> minute part as an integer
	.				       ;   `#3600` removes hours, remaining value `/60` gives minutes
	. ; Its possible to get 60 with the above rounding in such cases increment hour and SET minute to 0.
	. ; We will not exceed -12 and 14 in such case as the value recieved is ensured by parser to not exceed
	. ; this range.
	. ; Following query can result in such a case
	. ; 	select timestamp(zhorolog) with time zone '2980013,86399,9,-50399';
	. SET:(timezoneM=60) timezoneM=0,timezoneH=timezoneH+1
	. SET:(10>timezoneH) timezoneH="0"_timezoneH
	. SET:(10>timezoneM) timezoneM="0"_timezoneM
	. SET:(10>timezoneS) timezoneS="0"_timezoneS
	. SET timezone=$select(sign=1:"-",1:"+")_timezoneH_":"_timezoneM_$select("00"=timezoneS:"",1:":"_timezoneS) ; The sign is reversed here because timezone value of zhorolog is reverse of utc offSET
	IF (date=type) DO
	. ; Convert "date" type HOROLOG value to text format
	. SET result=$ZDATE(inputStr,"MM/DD/YEAR")
	ELSE  IF (time=type)!(timeWithTimeZone=type) DO
	. ; Convert "time" type HOROLOG value to text format
	. ; Note $ZDATE expects ",3661" syntax if only time value is given
	. SET result=$ZDATE(inputStr,"24:60:SS")
	. SET:(""'=microsec) result=result_"."_microsec
	. IF (timeWithTimeZone=type) DO
	. . SET result=result_timezone
	ELSE  IF (timestamp=type)!(timestampWithTimeZone=type) DO
	. ; Convert "timestamp" type HOROLOG value to text format
	. SET result=$ZDATE(inputStr,"MM/DD/YEAR 24:60:SS")
	. SET:(""'=microsec) result=result_"."_microsec
	. IF (timestampWithTimeZone=type) DO
	. . SET result=result_timezone
	QUIT result

Fileman2Text(inputStr,type)
	; Convert Fileman date and time to the same in text format
	; Manually convert Fileman date/time to date/time text value
	; `inputStr` format is expected to be `YYYMMDD.HHMMSS`
	; Based on `type` ("date"/"time"/"timeWithTimeZone"/"timestamp"/"timestampWithTimeZone") input will be processed
	; Output format is also determined by the type
	NEW result,length,i,isTime,timeCounter,dateCounter,endLoop
	SET (year,result,month,day,hour,minute,second)=""
	SET length=$length(inputStr)
	QUIT:7>length $ZYSQLNULL
	SET (isTime,timeCounter,dateCounter,endLoop)=0
	FOR i=1:1:length DO  QUIT:endLoop
	. SET x=$EXTRACT(inputStr,i)
	. IF ("."=x) SET isTime=1
	. ELSE  IF (isTime) DO
	. . ; first 2 is HH
	. . ; second 2 is MM
	. . ; third 2 is SS
	. . IF (timeCounter<2) SET hour=hour_x
	. . ELSE  IF (timeCounter<4) SET minute=minute_x
	. . ELSE  IF (timeCounter<6) SET second=second_x
	. . ELSE  SET endLoop=1
	. . SET timeCounter=timeCounter+1
	. ELSE  DO
	. . ; first 3 is YYY
	. . ; next 2 is MM
	. . ; next 2 is DD
	. . IF (dateCounter<3) SET year=year_x
	. . ELSE  IF (dateCounter<5) SET month=month_x
	. . ELSE  IF (dateCounter<7) SET day=day_x
	. . SET dateCounter=dateCounter+1
	; Handle in-exact dates here
	IF (year="000") DO
	. ; error
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	ELSE  IF (year="999")&(month="12")&(day="31")&("24"=hour) DO
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	ELSE  IF (month="00")&(day'="00") DO
	. ; error
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF (""=minute)&(""=second)&(2=$length(hour)) DO
	. IF (20=hour)!(10=hour) DO
	. . ; 10 and 20 are invalid for hour when minute and second is absent
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. . ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	SET:(month="00") month="01"
	SET:(day="00") day="01"
	SET:(hour="") hour="00"
	SET:(minute="") minute="00"
	SET:(second="") second="00"
	IF ("24"=hour) DO
	. SET day=day+1
	. ; Jan 31 Feb 28 (29) March 31 April 30 May 31 June 30 July 31 August 31 Sept 30 Oct 31 Nov 30 Dec 31
	. SET month=+month
	. SET addday=$select(1=month:day>31,2=month:day>29,3=month:day>31,4=month:day>30,5=month:day>31,6=month:day>30,7=month:day>31,8=month:day>31,9=month:day>30,10=month:day>31,11=month:day>30,12=month:day>31)
	. IF (0'=addday) DO
	. . SET month=month+1
	. . SET day=1
	. IF (13=month) DO
	. . SET year=year+1
	. . SET month=1
	. SET hour="00"
	IF (2'=$length(hour)) DO
	. IF (3>hour) SET hour=hour_"0" ; Hour can be single digit (2960124.1), add trailing 0 in this case
	. ELSE  DO
	. . ; A single digit hour value >=3 is invalid
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. . ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF (2'=$length(minute)) DO
	. IF (6>minute) SET minute=minute_"0" ; Minute can be single digit (2960124.162), add trailing 0 in this case
	. ELSE  DO
	. . ; A single digit minute value >=6 is invalid
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. . ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF (2'=$length(second)) DO
	. IF (6>second) SET second=second_"0" ; Seconds can be single digit (2960124.16263), add trailing 0 in this case
	. ELSE  DO
	. . ; A single digit seconds value >=6 is invalid
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",1)=inputStr  ; pass parameter to `src/ydb_error_check.c`
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",2)=type
	. . SET %ydboctoerror("INVALIDDATETIMEVALUE",3)=filemanFormat
	. . ZMESSAGE %ydboctoerror("INVALIDDATETIMEVALUE")
	IF (date=type) DO
	. ; Convert "date" type Fileman value to text format
	. SET year=year+1700
	. SET result=month_"/"_day_"/"_year
	. SET endLoop=1
	ELSE  IF (timestamp=type)!(timestampWithTimeZone=type) DO
	. ; Convert "timestamp" type Fileman value to text format
	. SET year=year+1700
	. SET result=month_"/"_day_"/"_year_" "_hour_":"_minute_":"_second
	. SET endLoop=1
	ELSE  SET result="",endLoop=1
	QUIT result

max(isString,a,b)
	; return the greatest of a and b
	; uses lexicographical sorting if `isString` is true; otherwise uses numerical sorting
	; if `a` is NULL, returns `b` (even if b is NULL)
	; invoked by GREATEST
	QUIT $SELECT($ZYISSQLNULL(a):b,$ZYISSQLNULL(b):a,isString:$SELECT(a]b:a,1:b),1:$SELECT(a>b:a,1:b))

min(isString,a,b)
	; return the least of a and b (see `max` for details)
	; invoked by LEAST
	QUIT $SELECT($ZYISSQLNULL(a):b,$ZYISSQLNULL(b):a,isString:$SELECT(a]b:b,1:a),1:$SELECT(a>b:b,1:a))

dollarZTRIGGER(arg1,arg2);
	; Helper M function invoked by generated M code whenever it needs to do a $ZTRIGGER call.
	SET status=$ZTRIGGER(arg1,arg2)
	QUIT status

UNIONALL(inputId1,inputId2,outputId)
	; Helper M function that does UNION ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z1,z2,zmax1,zmax2
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 into outputId
	SET zmax1=$GET(%ydboctocursor(cursorId,"keys",inputId1,"",""),0)
	FOR z1=1:1:zmax1 SET %ydboctocursor(cursorId,"keys",outputId,"","",z1)=%ydboctocursor(cursorId,"keys",inputId1,"","",z1)
	; Merge key corresponding to inputId2 into outputId
	SET zmax2=$GET(%ydboctocursor(cursorId,"keys",inputId2,"",""),0)
	FOR z2=1:1:zmax2 SET %ydboctocursor(cursorId,"keys",outputId,"","",zmax1+z2)=%ydboctocursor(cursorId,"keys",inputId2,"","",z2)
	; Set # of records in output table before returning
	SET %ydboctocursor(cursorId,"keys",outputId,"","")=zmax1+zmax2
	QUIT

UNION(inputId1,inputId2,outputId)
	; Helper M function that does UNION of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . QUIT:$DATA(index(val))
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	. . SET index(val)=""
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

INTERSECTALL(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(val))
	. . ELSE  IF +$GET(index(val)) DO
	. . . IF $INCREMENT(index(val),-1)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

INTERSECT(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) SET index(val)=""
	. . ELSE  IF $DATA(index(val)) DO
	. . . KILL index(val)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

EXCEPTALL(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(val))
	. . ELSE  IF +$GET(index(val)) DO
	. . . IF $INCREMENT(index(val),-1)
	SET subs=""
	FOR  SET subs=$ORDER(index(subs)) QUIT:subs=""  DO
	. FOR z=1:1:index(subs) DO
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=subs
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

EXCEPT(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) SET index(val)=""
	. . ELSE  KILL index(val)
	SET subs=""
	FOR  SET subs=$ORDER(index(subs)) QUIT:subs=""  DO
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=subs
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

columnkeyUNIONALL(inputId1,inputId2,outputId)
	; Helper M function that does UNION ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as UNIONALL^%ydboctoplanhelpers except that this operates on UNION ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement. The input key structure in
	; this case is of the form
	;	%ydboctocursor(cursorId,"keys",inputId,"","",value1)
	;	%ydboctocursor(cursorId,"keys",inputId,"","",value2)
	;	etc.
	; instead of the usual
	;	%ydboctocursor(cursorId,"keys",inputId,"","",1)
	;	%ydboctocursor(cursorId,"keys",inputId,"","",2)
	;	etc.
	;
	NEW id,subs,val
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF $INCREMENT(%ydboctocursor(cursorId,"keys",outputId,"","",subs),val)
	QUIT

columnkeyUNION(inputId1,inputId2,outputId)
	; Helper M function that does UNION of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as UNION^%ydboctoplanhelpers except that this operates on UNION operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	QUIT

columnkeyINTERSECTALL(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as INTERSECTALL^%ydboctoplanhelpers except that this operates on INTERSECT ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW index,val,val2,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(subs),val)
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET val2=+$GET(index(subs))
	. . . SET:(val>val2) val=val2
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=val
	QUIT

columnkeyINTERSECT(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as INTERSECT^%ydboctoplanhelpers except that this operates on INTERSECT operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW index,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . IF (id=inputId1) DO
	. . . SET index(subs)=1
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	QUIT

columnkeyEXCEPTALL(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as EXCEPTALL^%ydboctoplanhelpers except that this operates on EXCEPT ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW index,id,val,val2,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(subs),val)
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET val2=+$GET(index(subs))
	. . . SET:(val>val2) val=val2
	. . . IF $INCREMENT(index(subs),-val)
	SET subs=""
	FOR  DO:$DATA(index(subs))  SET subs=$ORDER(index(subs)) QUIT:subs=""
	. SET val=+$GET(index(subs))
	. QUIT:'val
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=val
	QUIT

columnkeyEXCEPT(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as EXCEPT^%ydboctoplanhelpers except that this operates on EXCEPT operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW index,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . IF (id=inputId1) DO
	. . . SET index(subs)=1
	. . ELSE  IF $GET(index(subs)) DO
	. . . KILL index(subs)
	SET subs=""
	FOR  DO:$DATA(index(subs))  SET subs=$ORDER(index(subs)) QUIT:subs=""
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	QUIT

GetScalarOrArray(keyId,toArray,planName)
	; Helper M function for processing scalar values and single-row arrays. The toArray parameter indicates whether or not to
	; produce a scalar value or compose an array based on the provided output key # (keyId). In either case, the return is a
	; single value. In the array case, this return value is a string in PostgreSQL array format, i.e. {elem1,elem2,...}. In the
	; scalar case, this routine checks if the output key has at most one row (guaranteed to have only one column at parse time).
	; If so it returns that as the value. If not, it issues an error.
	;
	; Note that the array case permits multiple rows to be returned, per the PostgreSQL specification for ARRAY constructors
	; using subqueries at https://www.postgresql.org/docs/11/sql-expressions.html#SQL-SYNTAX-ARRAY-CONSTRUCTORS.
	;
	; This routine is used by generated plans where a sub-query is used in place of a scalar value (e.g. arithmetic expression
	; etc.). Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW firstsub,secondsub,morethanonesub,curvalue,result
	; The variable planName points to the physical plan entryref that needs to be invoked first (in case it is a deferred plan)
	;   in order to generate the output key rows. Example value is "octoPlan2^%ydboctoPrTrjCuwSxj7urDaUaUSh1G"). The
	;   resulting output key rows are used to obtain the return value of this function call.
	;   It is "" in case the plan is not a deferred plan. And in this case, no physical plan entryref needs to be invoked.
	IF $$InvokeOctoPlan(planName)
	; Check if there are no rows in subquery output. If so we should return NULL per SQL standard.
	QUIT:(1>=$DATA(%ydboctocursor(cursorId,"keys",keyId,"",""))) $ZYSQLNULL
	SET firstsub=$SELECT($DATA(%ydboctocursor(cursorId,"keys",keyId,"","","")):"",1:$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","","")))
	; Find out if the output key has more than one row. If so issue an error
	; Note that it is possible the same row gets duplicated more than once. In that case though
	; the node value would be greater than 1. So check that too (in addition to checking $ORDER returns "").
	; It is possible firstsub and secondsub are both "" in which case there is only one sub. Check for that.
	; Note that if firstsub is $ZYSQLNULL, then we are guaranteed there is no second subscript (since
	IF (toArray) DO
	. SET result="{"
	. SET %ydboctocursor(cursorId,"keys",keyId,"","")=""
	. FOR  SET %ydboctocursor(cursorId,"keys",keyId,"","")=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"",""))) QUIT:(%ydboctocursor(cursorId,"keys",keyId,"","")="")  DO
	. . IF ($ZYISSQLNULL($$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"",""))))) DO
	. . . SET result=result_"NULL,"
	. . ELSE  IF (""=$$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"","")))) DO
	. . . SET result=result_""""","
	. . ELSE  DO
	. . . SET result=result_$$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"","")))_","
	. SET result=$EXTRACT(result,1,$LENGTH(result)-1)_"}"
	ELSE  DO
	. SET result=firstsub
	. SET secondsub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	. SET morethanonesub=$SELECT($ZYISSQLNULL(firstsub):0,$ZYISSQLNULL(secondsub):1,'$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",secondsub)):0,1:(firstsub'=secondsub))
	. SET:'morethanonesub morethanonesub=(1<%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	. ZMESSAGE:morethanonesub %ydboctoerror("SUBQUERYMULTIPLEROWS")
	QUIT result ; Return scalar in only column and only row of keyId

EXISTS(keyId,planName)
	; Helper M function that given an output key # (keyId) checks if the output key has at least one row
	; If so returns 1 and if not returns 0. Implements the EXISTS operator in SQL.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
	QUIT (1<$DATA(%ydboctocursor(cursorId,"keys",keyId,"","")))

ANY(inputValue,keyId,compOp,isString,planName,convertDateTime,type)
	; Helper M function that implements the ANY/SOME operator in SQL.
	; Given an output key # (keyId) checks if the output key has at least one row with a value that satisfies the
	;   compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns TRUE and at
	;   least one of the return is NULL (in accordance with SQL rules for NULL).
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
	NEW ret,sub,lclSub,doConvertDT,dtType
	SET sub="",ret=0
	SET doConvertDT=$GET(convertDateTime,0)
	SET dtType=$GET(type,"")
	FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",sub))  SET sub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",sub)) QUIT:ret!(""=sub)
	. SET lclSub=sub
	. SET:doConvertDT lclSub=$$ConvertToLocalTimezone(type,sub)
	. SET ret=$$Compare(inputValue,compOp,lclSub,isString)
	QUIT ret

ALL(inputValue,keyId,compOp,isString,planName,convertDateTime,type)
	; Helper M function that implements the ALL operator in SQL.
	; Given an output key # (keyId) checks if the output key has ALL rows with a value that satisfies the
	;   compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns FALSE and at
	;   least one of the return is NULL (in accordance with SQL rules for NULL).
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
	NEW ret,sub,lclSub,doConvertDT,dtType
	SET sub="",ret=1
	SET doConvertDT=$GET(convertDateTime,0)
	SET dtType=$GET(type,"")
	FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",sub))  SET sub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",sub)) QUIT:'ret!(""=sub)
	. SET lclSub=sub
	. SET:doConvertDT lclSub=$$ConvertToLocalTimezone(type,sub)
	. SET ret=$$Compare(inputValue,compOp,lclSub,isString)
	QUIT ret

Compare(value1,compOp,value2,isString)
	; Helper M function used by $$ANY and $$ALL to perform comparison
	NEW ret
	; If value is a STRING type, then simple M "=" or "'=" operators are okay. Otherwise, numeric comparison needs a
	; coercion of the operand to a number hence the use of "+" on the operands before doing the "=" or "'=" below (YDBOcto#574).
	QUIT:("="=compOp) $SELECT(isString:value1=value2,1:$$ForceNumeric(value1)=$$ForceNumeric(value2))
	QUIT:("'="=compOp) $SELECT(isString:value1'=value2,1:$$ForceNumeric(value1)'=$$ForceNumeric(value2))
	IF 'isString  DO  QUIT ret
	. SET:("<"=compOp) ret=(value1<value2)
	. SET:("<="=compOp) ret=(value1<=value2)
	. SET:(">"=compOp) ret=(value1>value2)
	. SET:(">="=compOp) ret=(value1>=value2)
	; Now that we know it is a string type and we have inequality checks, we need to figure out
	; the right operator to use (M FOLLOWS operator or its complement).
	QUIT:(">"=compOp) value1]value2
	QUIT:("<="=compOp) value1']value2
	QUIT:(">="=compOp) value2']value1
	QUIT:("<"=compOp) value2]value1
	QUIT  ; We do not expect to reach here. Hence the QUIT without any value (will generate a runtime error).

ForceNumeric(value)
	QUIT +value

; The following routine is only used by ValidateInputAndGetTrimdVal to issue INVALIDINTEGERSYNTAX or INVALIDNUMERICSYNTAX error
; `type` is used to choose the error to be issued
IssueInvalidInputSyntaxError(value,type)
	NEW errorStr
	IF ("integer"=type) set errorStr="INVALIDINTEGERSYNTAX"
	ELSE  SET errorStr="INVALIDNUMERICSYNTAX" ; ("numeric"=type)
	SET %ydboctoerror(errorStr,1)=value       ; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror(errorStr)
	QUIT

; Following routine validates the passed `value` and returns the casted value
String2Integer(value)
	SET value=$$ValidateInputAndGetTrimdVal(value,"integer")
	QUIT value\1

; Input:
;	value : string with the value to be casted
;	type : the type to which value needs to be casted to. It is expected to have just "integer"/"numeric"
; Output:
;	returns passed input value but without leading white spaces
; The function checks whether the passed `value` is compatible for an INTEGER/NUMERIC cast or not.
; If the input `value` is found to be invalid, IssueInvalidInputSyntaxError is invoked to issue type specific error.
; `type` is expected to determine the conditions to find invalidity.
;  The following conditions are ensured to be TRUE by the routine
; 1. Input doesn't have only '+' or '-' alone in input string i.e. '+', '+  '  +',..
; 2. Input doesn't have period alone in the input string i.e. '.', '.  ',' .',..
; 3. Input doesn't have just white spaces i.e. '            '
; 4. Input doesn't have non integer or numeric characters in the input string i.e. 'abc','1.abc','1. 1',..
; 5. Input doesn't have period when the conversion is to an integer
; 6. Input doesn't have '+' or '-' where it is not expected i.e. '123+','12+2',..
ValidateInputAndGetTrimdVal(value,type)
	QUIT:$ZYISSQLNULL(value) $ZYSQLNULL	; Possible with an input of the form `(NULL::varchar)::integer`
	NEW length,i,curChar,startIndex,firstSpaceAfterValidChar,firstMinusPlusReached,firstIntegerReached
	SET (firstDotReached,firstSpaceAfterValidChar,firstMinusPlusReached,firstIntegerReached,startIndex)=0
	; Note startIndex is set to the first validchar index in the below FOR loop
	SET length=$ZLENGTH(value)
	FOR i=1:1:length DO
	. SET curChar=$ZEXTRACT(value,i)
	. ; Skip all leading white space
	. QUIT:('startIndex&(curChar=" "))
	. IF ('startIndex) DO
	. . ; Following condition check if the character is a
	. . ; 1. '+' or '-' sign
	. . ; 2. '.' usage
	. . ; 3. integer value between 0-9
	. . IF (("numeric"=type)&("."=curChar)) SET firstDotReached=1,startIndex=i	; first '.' usage
	. . ELSE  IF (("+"=curChar)!("-"=curChar)) SET firstMinusPlusReached=1,startIndex=i
	. . ELSE  IF (("0"']curChar)&(curChar']"9")) SET firstIntegerReached=1,startIndex=i
	. . ELSE  DO IssueInvalidInputSyntaxError(value,type)
	. ELSE  DO
	. . ; Following takes care of trailing white spaces. We set `firstSpaceAfterValidChar` to 1 when we encounter a space after
	. . ; a valid character is seen in the string. This value is used in the else block to skip trailing white spaces and issue
	. . ; error if trailing white space is inbetween valid chars.
	. . IF ((" "=curChar)&(0=firstSpaceAfterValidChar)) SET firstSpaceAfterValidChar=1
	. . ELSE  DO
	. . . IF ((" "=curChar)&(1=firstSpaceAfterValidChar)) QUIT  ; Nothing to do as this may be trailing white space
	. . . ELSE  IF (1=firstSpaceAfterValidChar) DO
	. . . . ; White space found in between (eg: '12 12'). This is invalid issue an error.
	. . . . DO IssueInvalidInputSyntaxError(value,type)
	. . . ELSE  IF '(("0"']curChar)&(curChar']"9")) do
	. . . . ; Value is not 0-9
	. . . . IF ((("numeric"=type)&("."=curChar))&('firstDotReached)) SET firstDotReached=1 ; first '.' usage
	. . . . ELSE  DO IssueInvalidInputSyntaxError(value,type)
	. . . ELSE  SET:(0=firstIntegerReached) firstIntegerReached=1
	; All white spaces or only . or only + or only - exists without an integer following it
	; This is invalid issue an error
	DO:(('startIndex)!((firstMinusPlusReached!firstDotReached)&'firstIntegerReached)) IssueInvalidInputSyntaxError(value,type)
	QUIT $SELECT((1=startIndex):value,1:$ZEXTRACT(value,startIndex,length))

CountAsterisk(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT(*) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex` (unused)
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	; Since COUNT(*) does not examine values for NULL, no need to use `curValue` in this case.
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex))
	QUIT

Count(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex))
	QUIT

Min(keyId,groupBySubs,aggrIndex,curValue,isString)
	; Helper M function to implement the MIN() and MIN(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	;	isString    : 1 if curValue is of string type, 0 if curValue is of numeric type
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	NEW curMin
	QUIT:$ZYISSQLNULL(curValue)
	IF $DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)) DO
	. ; Values have already been aggregated. Compare current value against stored min value and update if needed.
	. SET curMin=%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)
	. IF (('isString&(curValue<curMin))!(isString&(curMin]curValue))) DO
	. . SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	ELSE  DO
	. ; No values have been aggregated yet. Current value is the MIN.
	. SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	QUIT

Max(keyId,groupBySubs,aggrIndex,curValue,isString)
	; Helper M function to implement the MAX() and MAX(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	;	isString    : 1 if curValue is of string type, 0 if curValue is of numeric type
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	NEW curMax
	QUIT:$ZYISSQLNULL(curValue)
	IF $DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)) DO
	. ; Values have already been aggregated. Compare current value against stored max value and update if needed.
	. SET curMax=%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)
	. IF (('isString&(curValue>curMax))!(isString&(curValue]curMax))) DO
	. . SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	ELSE  DO
	. ; No values have been aggregated yet. Current value is the MAX.
	. SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	QUIT

Sum(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the SUM() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex),curValue)
	QUIT

Avg(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the AVG() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	; Note: We store the cumulative sum and the cumulative count here. Actual average will be taken in generated M code
	;	(code generation happens in `src/m_templates/tmpl_print_expression.ctemplate` in the LP_AGGREGATE_FUNCTION_AVG
	;	and LP_AGGREGATE_FUNCTION_AVG_DISTINCT switch/case blocks).
	; If we use `aggrIndex` (without the minus sign prefix), we could encounter an issue.
	;	This is because in the case of AVG(DISTINCT), the label `AvgDistinct` could encounter curValue as "SUM" or "COUNT"
	;	in which case it is going to set that as a subscript underneath the `aggrIndex` subscript in which case, it
	;	would confuse the "SUM"/"COUNT" maintenance for AVG. Hence the choice of a negative subscript.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,-aggrIndex,"SUM"),curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,-aggrIndex,"COUNT"))
	QUIT

CountDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Count(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

SumDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the SUM(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Sum(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

AvgDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the AVG(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Avg(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

Integer2Boolean(intvalue)	;
	; Converts an input boolean parameter (`intvalue`) to 1 if it evaluates to a non-zero value and 0 otherwise.
	; This is so we are compatible with Postgres
	QUIT $SELECT($ZYISSQLNULL(intvalue):$ZYSQLNULL,+intvalue:1,1:0)

Boolean2String(boolvalue)	;
	; Converts an input boolean parameter (`boolvalue`) to `true` if it evaluates to a non-zero value and `false` otherwise
	; This is so we are compatible with Postgres
	QUIT $SELECT($ZYISSQLNULL(boolvalue):$ZYSQLNULL,+boolvalue:"true",1:"false")

String2Boolean(boolstr)	;
	; Converts an input boolean string value (`boolstr`) (can be `t` or `f`) to 1 or 0 respectively
	QUIT:$ZYISSQLNULL(boolstr) $ZYSQLNULL
	IF '$DATA(%ydboctoStr2Bool) DO
	.	; Below are list of string literals which are accepted for boolean conversion
	.	; A similar list is maintained in a switch/case block in the "literal_value" rule definition in src/parser.y
	.	SET %ydboctoStr2Bool("true")=1
	.	SET %ydboctoStr2Bool("t")=1
	.	SET %ydboctoStr2Bool("yes")=1
	.	SET %ydboctoStr2Bool("y")=1
	.	SET %ydboctoStr2Bool("1")=1
	.	SET %ydboctoStr2Bool("false")=0
	.	SET %ydboctoStr2Bool("f")=0
	.	SET %ydboctoStr2Bool("no")=0
	.	SET %ydboctoStr2Bool("n")=0
	.	SET %ydboctoStr2Bool("0")=0
	IF '$DATA(%ydboctoStr2Bool(boolstr))  DO
	.	SET %ydboctoerror("INVALIDBOOLEANSYNTAX",1)=boolstr	; pass parameter to `src/ydb_error_check.c`
	.	ZMESSAGE %ydboctoerror("INVALIDBOOLEANSYNTAX")
	QUIT %ydboctoStr2Bool(boolstr)

str2mval(str)
	; Converts an input string `str` into a `len,str` 2-tuple. Cannot use `str` as is in case input string is `$ZYSQLNULL`
	; (as that cannot then be later used as part of a concatenation operation since the result of the concatenation
	; would be $ZYSQLNULL losing all other operands of the concatenation operator).
	;
	; The length can be 1 or 2 or 3 bytes long.
	;
	;	String	Output
	;	------	-------------
	;	NULL    00000000
	;	1-byte  00000001                    to 01111110                   [ String Byte Lengths of 0 to 126 + 1-byte header length = length of 1 to 127]
	;	2-byte  10000000 10000001           to 10111111 11111111          [ String Byte Lengths of 127 to 16381 + 2-byte header length = length of 129 to 16383]
	;	3-byte  11000000 01000000 00000001  to 11010000 00000000 00000000 [ String Byte Lengths of 16382 to 1048573 + 3-byte header length = length of 16385 + 1048576]
	;
	QUIT:$ZYISSQLNULL(str) $ZCHAR(0)
	NEW len,hdr
	SET len=$ZLENGTH(str)
	IF 127>len DO
	.	; 1-byte header
	. 	SET hdr=$ZCHAR(1+len)
	ELSE  IF 16382>len DO
	.	; 2-byte header
	.	SET len=len+2	; Add 2-byte header length too
	.	SET hdr=$ZCHAR(128+(len\256))_$ZCHAR(len#256)
	ELSE  DO
	.	; 3-byte header
	.	SET len=len+3	; Add 3-byte header length too
	.	SET hdr=$ZCHAR(192+(len\65536))_$ZCHAR((len\256)#256)_$ZCHAR(len#256)
	QUIT hdr_str

mval2str(mval)
	; Converts the input `mval` (`len,str` tuple) back into a string (just `str`) and returns that.
	; This is the inverse of $$str2mval^%ydboctoplanhelpers.
	;
	NEW byte1,hdrlen,datalen
	SET byte1=$ZASCII($ZEXTRACT(mval,1))
	; If mval is the empty string, $ZEXTRACT would return the empty string and $ZASCII would return -1 ("byte1" == -1).
	; In that case, we should treat the mval as the empty string and return $ZYSQLNULL as the corresponding string value.
	; In the case byte1 is 0, we want to return $ZYSQLNULL.
	; Hence the below check for whether "byte1" is less than or equal to 0.
	QUIT:0>=byte1 $ZYSQLNULL
	IF 128>byte1 DO
	.	; 1-byte header
	.	SET hdrlen=1
	.	SET datalen=byte1-1
	ELSE  IF 192>byte1 DO
	.	; 2-byte header
	.	SET hdrlen=2
	.	SET datalen=$$get2bytedatalen(byte1,mval,0)
	ELSE  DO
	.	; 3-byte header
	.	SET hdrlen=3
	.	SET datalen=$$get3bytedatalen(byte1,mval,0)
	QUIT $ZEXTRACT(mval,hdrlen+1,hdrlen+datalen)

mvalPiece(mval,piecenum)
	; Locates the `piecenum`th piece in `mval` (a concatenated sequence of `mval`s) and returns that `mval (`len,str`)
	;
	; Note: `src/get_mval_len.c` has very similar logic as below. So any changes here need to be reflected there as well.
	;
	NEW byte1,i,hdrlen,datalen,cumullen
	SET cumullen=0
	FOR i=1:1:piecenum DO  QUIT:i=piecenum  IF $INCREMENT(cumullen,hdrlen+datalen)
	.	SET byte1=$ZASCII($ZEXTRACT(mval,cumullen+1))
	.	IF 0=byte1 DO
	.	.	; $ZYSQLNULL
	.	.	SET hdrlen=1
	.	.	SET datalen=0
	.	ELSE  IF 128>byte1 DO
	.	.	; 1-byte header
	.	.	SET hdrlen=1
	.	.	SET datalen=byte1-1
	.	ELSE  IF 192>byte1 DO
	.	.	; 2-byte header
	.	.	SET hdrlen=2
	.	.	SET datalen=$$get2bytedatalen(byte1,mval,cumullen)
	.	ELSE  DO
	.	.	; 3-byte header
	.	.	SET hdrlen=3
	.	.	SET datalen=$$get3bytedatalen(byte1,mval,cumullen)
	QUIT $ZEXTRACT(mval,cumullen+1,cumullen+hdrlen+datalen)

get2bytedatalen(byte1,mval,offset)
	; Computes the length given a 2-byte header (1st byte is already in byte1 and 2nd byte is obtained from mval)
	; `offset` indicates how many bytes to go past before extracting the 2nd byte
	QUIT (byte1-128)*256+$ZASCII($ZEXTRACT(mval,offset+2))-2

get3bytedatalen(byte1,mval,offset)
	; Computes the length given a 3-byte header (1st byte is already in byte1 and 2nd/3rd bytes are obtained from mval)
	; `offset` indicates how many bytes to go past before extracting the 2nd/3rd byte
	QUIT (byte1-192)*65536+($ZASCII($ZEXTRACT(mval,offset+2))*256)+$ZASCII($ZEXTRACT(mval,offset+3))-3

piecevalue2colvalue(piecevalue)
	; Check if "piecevalue" is not empty string. If so, return that right away.
	QUIT:(""'=piecevalue) piecevalue
	; Now that we know "piecevalue" is "" (i.e. empty string), return M equivalent of SQL NULL.
	QUIT $ZYSQLNULL

colvalue2piecevalue(colvalue)
	; Inverse of "piecevalue2colvalue()"
	;
	; First handle case where "colvalue" is not $ZYSQLNULL (the M equivalent of SQL NULL).
	QUIT:'$ZYISSQLNULL(colvalue) colvalue
	; Now we know "colvalue" is $ZYSQLNULL. Return empty string.
	QUIT ""

trimdotstar(resstr)
	; Removes consecutive .*'s present in resstr
	; Example: .*.* -> .*
	NEW trim,result,len
	SET trim=0,result=""
	SET len=$ZLENGTH(resstr)
	FOR i=1:1:len  DO
	. SET ch=$ZEXTRACT(resstr,i)
	. IF "."=ch DO
	. . SET ch=$ZEXTRACT(resstr,i+1)
	. . IF "*"=ch DO
	. . . DO:1'=trim
	. . . . SET trim=1
	. . . . SET result=result_".*"
	. . . SET i=i+1
	. . ELSE  DO
	. . . SET trim=0
	. . . SET result=result_"."
	. ELSE  DO
	. . SET trim=0
	. . SET result=result_ch
	QUIT result

regexinit
	; Initialize the regex transformation local variables once per octo process.
	; Comments at the beginning of routine regexmatch explains why and how the transformations
	; arrays below are used.
	DO:(0=$GET(%ydboctoregex(0,1,"%"),0))
	. ; LIKE
	. SET %ydboctoregex(0,1,"%")=".*",%ydboctoregex(0,1,"_")=".",%ydboctoregex(0,1,".")="\.",%ydboctoregex(0,1,"*")="\*"
	. SET %ydboctoregex(0,1,"[")="\[",%ydboctoregex(0,1,"]")="\]",%ydboctoregex(0,1,"+")="+",%ydboctoregex(0,1,"?")="?"
	. SET %ydboctoregex(0,1,"{")="{",%ydboctoregex(0,1,"}")="}",%ydboctoregex(0,1,"(")="(",%ydboctoregex(0,1,")")=")"
	. SET %ydboctoregex(0,1,"|")="|"
	. ; SIMILAR TO
	. SET %ydboctoregex(0,2,"%")=".*",%ydboctoregex(0,2,"_")=".",%ydboctoregex(0,2,".")="\.",%ydboctoregex(0,2,"*")="*"
	. SET %ydboctoregex(0,2,"[")="[",%ydboctoregex(0,2,"]")="]",%ydboctoregex(0,2,"+")="\+",%ydboctoregex(0,2,"?")="\?"
	. SET %ydboctoregex(0,2,"{")="\{",%ydboctoregex(0,2,"}")="\}",%ydboctoregex(0,2,"(")="\(",%ydboctoregex(0,2,")")="\)"
	. SET %ydboctoregex(0,2,"|")="$\|^"
	. ; ~
	. SET %ydboctoregex(0,3,"%")="%",%ydboctoregex(0,3,"_")="_",%ydboctoregex(0,3,".")=".",%ydboctoregex(0,3,"*")="*"
	. SET %ydboctoregex(0,3,"[")="[",%ydboctoregex(0,3,"]")="]",%ydboctoregex(0,3,"+")="\+",%ydboctoregex(0,3,"?")="\?"
	. SET %ydboctoregex(0,3,"{")="\{",%ydboctoregex(0,3,"}")="\}",%ydboctoregex(0,3,"(")="\(",%ydboctoregex(0,3,")")="\)"
	. SET %ydboctoregex(0,3,"|")="\|"
	. ; LIKE, SIMILAR TO & ~ operation with an escaped character
	. SET %ydboctoregex(1,"%")="%",%ydboctoregex(1,"_")="_",%ydboctoregex(1,"|")="|",%ydboctoregex(1,"+")="+"
	. SET %ydboctoregex(1,"?")="?",%ydboctoregex(1,"{")="{",%ydboctoregex(1,"}")="}",%ydboctoregex(1,"(")="("
	. SET %ydboctoregex(1,")")=")",%ydboctoregex(1,"\")="\\",%ydboctoregex(1,".")="\.",%ydboctoregex(1,"*")="\*"
	. SET %ydboctoregex(1,"[")="\[",%ydboctoregex(1,"]")="\]"
	QUIT

throwregexerr(regexstr)
	SET %ydboctoerror("INVALIDESCAPEPATTERN",1)=regexstr  ; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("INVALIDESCAPEPATTERN")
	; QUIT will not be reached as ZMESSAGE triggers error handling and returns control out of M code back to caller C code
	QUIT

pattransform(patt,type)
	; transform a pattern with appropriate escapes
	; type=1 transformation for LIKE
	; type=2 transfoprmation for SIMILAR TO
	; type=3 transformation for ~
	NEW len,ch,res,resstr
	SET len=$ZLENGTH(patt),resstr=""
	FOR i=1:1:len  DO
	. SET (res,ch)=$ZEXTRACT(patt,i)
	. IF "\"=ch DO
	. . ; escape character encountered
	. . IF ((i+1)>len) DO
	. . . IF (1=type) DO
	. . . . SET %ydboctoregex("result")="error"
	. . . . DO throwregexerr(patt)
	. . . ELSE  SET res="\"
	. . ELSE  DO
	. . . SET ch=$ZEXTRACT(patt,i+1),i=i+1
	. . . SET res=$GET(%ydboctoregex(1,ch),0)
	. . . SET:0=res res="\"_ch
	. ELSE  DO
	. . SET res=$GET(%ydboctoregex(0,type,ch),0)
	. . SET:0=res res=ch
	. SET resstr=resstr_res
	QUIT resstr

regexmatch(str,regexstr,regextype,regexflags)
	; By default, regex engine ($$regmatch^ydbposix) treats some metacharacters as off.
	; Where as SIMILAR TO and LIKE by default are expected to have these metacharacters to be on.
	; In order to use regex engine to process all regex pattern operations, regexstr is processed
	; to enable necessary metacharacters based on its operation.
	; Example:
	; 	Posix regex engine by default doesn't treat + with special meaning.
	;	But, \+ provides the character special meaning.
	;	SQL standard by default treats + with special meaning i.e using literal character +
	;	means match one or more units in case of SIMILAR TO and ~.
	;	To acheive synchronization between SQL standard and Posix regex engine we transform
	;	+ in a SQl query to \+ (SIMILAR TO & ~ usage) to treat it with special meaning.
	; This operation is done here as column values are not available in earlier stages and we
	; need pattern strings passed as column reference also to be processed.
	; Parameter:
	; 	str - left operand of regex operation
	;	regexstr   - right operand of regex operation (pattern string)
	; 	regextype   -  represents operation : 1->REGEX_LIKE (LIKE) ,2->REGEX_SIMILARTO (SIMILAR TO) & 3->REGEX_TILDE (~)
	;	regexflags -  if defined acts as the third argument for regex engine (see "man regcomp" for details on the below)
	;		1 => REG_EXTENDED	; use extended regular expression syntax (default is basic regex syntax)
	;		2 => REG_ICASE		; ignore case when matching (default is to not ignore case)
	;		3 => REG_EXTENDED and REG_ICASE are both enabled
	;		etc.
	;
	; regexstr is processed using the following rules:
	; Like			Similar TO			~
	; Input	Output		Input	Output		Input	Output
	; %	.*		%	.*		%	%
	; _	.		_	.		_	_
	; .	\.		.	\.		.	.
	; *	\*		*	*		*	*
	; [	\[		[	[		[	[
	; ]	\]		]	]		]	]
	; +	+		+	\+		+	\+
	; ?	?		?	\?		?	\?
	; {	{		{	\{		{	\{
	; }	}		}	\}		}	\}
	; (	(		(	\(		(	\(
	; )	)		)	\)		)	\)
	; |	|		|	$\|^		|	\|
	; \%	%		\%	%		\%	%
	; \_	_		\_	_		\_	_
	; \|	|		\|	|		\|	|
	; \+	+		\+	+		\+	+
	; \?	?		\?	?		\?	?
	; \{	{		\{	{		\{	{
	; \}	}		\}	}		\}	}
	; \(	(		\(	(		\(	(
	; \)	)		\)	)		\)	)
	; \\	\\		\\	\\		\\	\\
	; \.	\.		\.	\.		\.	\.
	; \*	\*		\*	\*		\*	\*
	; \[	\[		\[	\[		\[	\[
	; \]	\]		\]	\]		\]	\]
	;
	; if any of the input strings are $ZYSQLNULL return without processing
	QUIT:$ZYISSQLNULL(regexstr) $ZYSQLNULL
	QUIT:$ZYISSQLNULL(str) $ZYSQLNULL
	; optimization:
	; 	fetch previous regexstr, regextype and transformed regexstr("result")
	; 	avoid performing transformation again when current and previous regexstr and regextype are same
	NEW pvstr,pvrs,pvpln,ret
	SET pvstr=$GET(%ydboctoregex("regexstr"))
	SET pvrs=$GET(%ydboctoregex("result"))
	SET pvpln=$GET(%ydboctoregex("plan"))
	IF ((regexstr=pvstr)&(regextype=pvpln)) DO  QUIT ret
	. IF ("error"=pvrs) DO throwregexerr(regexstr)
	. ELSE  IF (".*"=pvrs) SET ret=1
	. ELSE  IF (0=$DATA(regexflags)) SET ret=$$regmatch^%ydbposix(str,pvrs)
	. ELSE  SET ret=$$regmatch^%ydbposix(str,pvrs,regexflags)
	SET %ydboctoregex("regexstr")=regexstr
	SET %ydboctoregex("plan")=regextype
	DO regexinit
	NEW resstr,result	; result will hold transformed regexstr
	SET resstr=""
	; anchor incase of SIMILAR TO and LIKE operation
	SET:regextype'=3 resstr="^"
	SET resstr=resstr_$$pattransform(regexstr,regextype)
	; anchor in case of LIKE and SIMILAR TO
	SET:3'=regextype resstr=resstr_"$"
	SET ret=0,result=resstr
	DO:3=regextype
	. ; .*.* type of regexstr can only occur in ~ operation
	. ; trim .* as its processing is faster here than in regex engine
	. SET result=$$trimdotstar(resstr)
	. IF (".*"=result) SET %ydboctoregex("result")=result,ret=1
	QUIT:1=ret 1
	SET %ydboctoregex("result")=result
	IF (0=$DATA(regexflags)) SET ret=$$regmatch^%ydbposix(str,result)
	ELSE  SET ret=$$regmatch^%ydbposix(str,result,regexflags)
	QUIT ret

Cast2VARCHAR(string,size)
	; This function helps implement the typecast operator where the target type is VARCHAR
	; "string" is the input string that needs to be type cast.
	; "size" is the maximum character length (not byte length) of the target string.
	; e.g. 'abcd'::VARCHAR(2) should return 'ab'
	QUIT:$ZYISSQLNULL(string) $ZYSQLNULL
	QUIT $EXTRACT(string,1,size)

String2NUMERIC(number,precision,scale)
	NEW result
	SET number=$$ValidateInputAndGetTrimdVal(number,"numeric")
	IF ($DATA(precision)) do
	. IF ($DATA(scale)) do
	. . SET result=$$Cast2NUMERICWithPrecision(number,precision,scale)
	. ELSE  SET result=$$Cast2NUMERICWithPrecision(number,precision)
	ELSE  DO
	. SET result=$$Cast2NUMERICWithoutPrecision(number)
	QUIT result

Cast2NUMERICWithPrecision(number,precision,scale)
	QUIT:$ZYISSQLNULL(number) $ZYSQLNULL
	; This function helps implement the typecast operator where the target type is NUMERIC
	; "number" is the input number that needs to be type cast.
	; "precision" is the maximum precision (i.e. total count of significant digits on either side of the decimal point)
	;	of the target number.
	; "scale" is the maximum scale (i.e. count of decimal digits to the right side of the decimal point) of the target number.
	; e.g. 15.54::NUMERIC(3,1) should return '15.5'
	; e.g. 15.54::NUMERIC(3,0) should return '16'
	; e.g. 15.54::NUMERIC(4,1) should return '15.5'
	; e.g. 0::NUMERIC(20,2) should return '0', not '0.00'
	NEW tmpnumber,tmpprecision
	SET tmpprecision=precision
	SET:'$DATA(scale) scale=0
	SET number=$FNUMBER(number,"",scale)	; If number has more digits after the decimal point than scale, truncate/round it
	SET tmpprecision=tmpprecision-scale
	SET tmpnumber=number\1		; Remove fractional part
	SET:(0>tmpnumber) tmpnumber=-tmpnumber ; Get absolute value (if negative)
	SET:(0>tmpprecision) tmpprecision=0	; If precision is negative, set it to 0
	DO:tmpnumber>=(10**tmpprecision)
	.	SET %ydboctoerror("NUMERICOVERFLOW",1)=precision	; pass parameter to `src/ydb_error_check.c`
	.	SET:'$DATA(scale) scale=0
	.	SET %ydboctoerror("NUMERICOVERFLOW",2)=scale		; pass parameter to `src/ydb_error_check.c`
	.	SET %ydboctoerror("NUMERICOVERFLOW",3)=tmpprecision	; pass parameter to `src/ydb_error_check.c`
	.	ZMESSAGE %ydboctoerror("NUMERICOVERFLOW")
	QUIT $$ForceNumeric(number)	; this is necessary to remove trailing 0s. e.g. to convert "2.00" to "2"

Cast2NUMERICWithoutPrecision(number)
	QUIT $SELECT($ZYISSQLNULL(number):$ZYSQLNULL,1:$$ForceNumeric(number))

SizeCheckVARCHAR(string,size)
	; This function is different from "Cast2VARCHAR" in that it issues an error if the "string" parameter does not fit
	; in the "size" parameter. This is invoked in the case of an INSERT INTO and an error is expected by the SQL standard
	; (the only exception is if the portion of the string greater than "size" characters is all spaces in which case the
	; excess spaces have to be removed and expected by the SQL standard).
	; Whereas the Cast2VARCHAR function is invoked by a type cast operator and truncation to fit the "size" (and not an error)
	; is expected by the SQL standard for the case where the "string" parameter does not fit in the "size" parameter.
	NEW charlen
	SET charlen=$LENGTH(string)
	QUIT:charlen<=size string
	; Check if excess portion is all spaces. If so, trim that out and return the trimmed string (no error).
	NEW excess,expected
	SET excess=$EXTRACT(string,size+1,charlen)
	SET expected=$JUSTIFY(" ",charlen-size)
	QUIT:excess=expected $EXTRACT(string,1,size)
	; Excess portion is not all spaces. This is an error scenario. Issue error.
	SET %ydboctoerror("VARCHARTOOLONG",1)=size	; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("VARCHARTOOLONG")
	QUIT

DuplicateKeyValue(name,detail)
	; This function is invoked to signal a UNIQUE constraint violation error.
	; "name" has the constraint name.
	; "detail" has additional detail on the actual values of the affected columns for the user.
	SET %ydboctoerror("DUPLICATEKEYVALUE",1)=""""_name_""" : "_detail_" already exists" ; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("DUPLICATEKEYVALUE")
	QUIT

NullColValue(colname)
	; This function is invoked to signal a NOT NULL constraint violation error.
	; "colname" has the column name.
	SET %ydboctoerror("NULLCOLVALUE",1)=colname	; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("NULLCOLVALUE")
	QUIT

CheckConstraintViolation(tablename,constraintname,numcols)
	; This function is invoked to signal a CHECK constraint violation error.
	; "tablename" has the table name
	; "constraintname" has the constraint name
	; "numcols" is the number of columns in the table (used to derive the column values of the violating row)
	NEW i,parm2
	SET %ydboctoerror("CHECKCONSTRAINTVIOLATION",1)=tablename ; pass parameter to `src/ydb_error_check.c`
	SET parm2=constraintname_" : Failing row contains ("
	FOR i=1:1:numcols  DO
	. NEW value
	. IF $data(colMetaData(i)) DO
	. . SET value=$$PrintDateTimeResultColumnValue(col(i),colMetaData(i,1),colMetaData(i,2),colMetaData(i,3))
	. ELSE  SET value=col(i)
	. SET parm2=parm2_$SELECT($ZYISSQLNULL(value):"NULL",1:value)_$SELECT(i=numcols:")",1:", ")
	SET %ydboctoerror("CHECKCONSTRAINTVIOLATION",2)=parm2
	ZMESSAGE %ydboctoerror("CHECKCONSTRAINTVIOLATION")
	QUIT

InvokeOctoPlan(planName)
	; Given a comma-separated list of plan names in "planName" (e.g. "octoPlan2,octoPlan3") this function invokes each of
	;   those plans and finally returns a value of 0. This is needed in cases where we want to invoke the plan (using
	;   "DO octoPlan2" etc.) but cannot do so because we are in the middle of an expression evaluation. Making it a
	;   function that returns a value of 0 allows us to use this function as the first choice of a $SELECT function call
	;   which always gets evaluated before processing the rest of the $SELECT function call (where the real processing
	;   happens based on the results of the execution of the input plan name).
	; Additionally a plan name can also contain a space-separated list of parameters corresponding to a SET operation.
	;   For example, a plan name in "planName" could be the string "SET 1 2 3 UNION". In this case, we are guaranteed
	;   the first word in the space-separated list is the string "SET" (see "tmpl_invoke_deferred_setoper.ctemplate"
	;   for the "InvokeDeferredPlan_EXISTS" case).
	; So the logic is to first extract each plan name using $PIECE() and the delimiter ",".
	;   And in the result check if the first space separated word is "SET". If so, it is a SET operation related invocation.
	;   Do SET related processing. If not, it is a direct octo plan invocation request. So do that instead.
	; Assumes "routine" and "cursorId" are appropriately set by caller.
	; Example values for routine is "%ydboctoP0sGaZQ410YcOGHn8750h9E" and "cursorId" is some integer like "10".
	NEW entryref,i,pieces
	SET pieces=$ZLENGTH(planName,",")
	; Since we will always have a trailing comma at the end, ignore the last empty piece. Hence the use of "pieces-1" below.
	FOR i=1:1:pieces-1 DO
	. SET entryref=$ZPIECE(planName,",",i)
	. IF "SET"=$ZPIECE(entryref," ",1) DO
	. . ; This is a SET operation type of request
	. . NEW inputId1,inputId2,outputId,mlabref
	. . SET inputId1=$ZPIECE(entryref," ",2)
	. . SET inputId2=$ZPIECE(entryref," ",3)
	. . SET outputId=$ZPIECE(entryref," ",4)
	. . SET mlabref=$ZPIECE(entryref," ",5)
	. . DO @mlabref@(inputId1,inputId2,outputId)
	. ELSE  DO
	. . ; This is an octo plan invocation type of request
	. . SET entryref=entryref_"^"_routine	; note: "routine" is a variable set in "src/aux/_ydboctoselect.m"
	. . DO @entryref@(cursorId)
	QUIT 0

InvokeSetOper(inputId1,inputId2,outputId,mlabref)
	; Just like "InvokeOctoPlan" entryref above helps invoke generated plan entryrefs, "InvokeSetOper" does it for SET
	; operations where the outputs of two operands of the SET operation (pointed to by "inputId1" and "inputId2") need
	; to be merged into the output table (pointed to by "outputId"). The entryref that implements the SET operation type
	; is pointed to by "mlabref".
	DO @mlabref@(inputId1,inputId2,outputId)
	QUIT 0

RowIsNull(mval,numCols)
	; Implements the IS NULL check for a row/record (potentially more than 1 column value).
	; Currently this is invoked only for the "t1.* IS NULL" case (where "t1.*" is a TABLENAME.ASTERISK syntax).
	; Input
	;   "mval" is a concatenated list of mvals representing the column values.
	;   "numCols" is the total number of column values in the row.
	; Returns
	;   1 if ALL columns values are NULL
	;   0 otherwise (i.e. if at least one column value is not NULL)
	NEW i,str,isNull
	SET isNull=1
	FOR i=1:1:numCols DO  QUIT:'isNull
	. SET str=$$mval2str($$mvalPiece(mval,i))
	. SET:'$ZYISSQLNULL(str) isNull=0
	QUIT isNull

RowIsNotNull(mval,numCols)
	; Implements the IS NOT NULL check for a row/record (potentially more than 1 column value).
	; Currently this is invoked only for the "t1.* IS NOT NULL" case (where "t1.*" is a TABLENAME.ASTERISK syntax).
	; Input
	;   "mval" is a concatenated list of mvals representing the column values.
	;   "numCols" is the total number of column values in the row.
	; Returns
	;   1 if ALL columns values are not NULL
	;   0 otherwise (i.e. if at least one column value is NULL)
	; Note that it is possible for both IS NULL and IS NOT NULL to return FALSE for a row if a row has
	; a mix of NULL and non-NULL values. So "NOT t1.* IS NULL" is not the same as "t1.* IS NOT NULL".
	; Hence the need for separate RowIsNull and RowIsNotNull entryrefs.
	NEW i,str,isNotNull
	SET isNotNull=1
	FOR i=1:1:numCols DO  QUIT:'isNotNull
	. SET str=$$mval2str($$mvalPiece(mval,i))
	. SET:$ZYISSQLNULL(str) isNotNull=0
	QUIT isNotNull

TableAsteriskCompare(firstOperand,secondOperand,operator,numColumns,colTypeList)
	; Input
	;   "firstOperand" and "secondOperand" is table.* values in the form col1val_col2val_.._colnval or
	;      $ZYSQLNULL in case of a composite NULL row.
	;   "numColumns" represents the number of columns present in table.
	;   "colTypeList" is a list of comma separated values representing column types.
	;      `'t'` represents a STRING_LITERAL type column. `'f'` represents
	;      all other types of columns. This is required to be able to apply $$ForceNumeric if the type is non-string.
	;   "operator" is a value representing any of the following characters `<`, `>`, `<=`, `>=`, `'=` and `=`.
	;      It is converted appropriately to an m operator by considering the operand type.
	; Returns
	;   1 or 0 based on comparison result
	;   $ZYSQLNULL if any of the operands have a $ZYSQLNULL value
	; Implementation Details
	;   This function performs the comparison as noted below:
	;   1. `=` : Returns on first FALSE comparison. If none exist the return value is TRUE.
	;   2. `'=`: Returns on first TRUE comparison. If none exist the return value is FALSE.
	;   3. `<` : If both operands are equal result is FALSE, continue to the next operand comparison. If first
	;            operand `<` second then exit with the result TRUE. If first operand `>` second then exit with the result FALSE.
	;   4. `<=`: Same as `<` but if both operands are equal result is TRUE.
	;   5. `>` : If both operands are equal result is FALSE, continue to the next operand comparison. If first
	;            operand `>` second then exit with the result TRUE. If first operand `<` second then exit with the result FALSE.
	;   6. `>=`: Same as `>` but if both operands are equal result is TRUE.
	; Note
	;   - Operator is in the form `<`,`>`,`<=`,`>=`,`'=` and `=`. Its converted appropriately to an m operator
	;       based on the operand type. For example `<` is `]` for a string operation where as for a numeric
	;       operation it is `<` itself.
	;   - In case of string operation the following operators `<`, `>`, `<=` and `>=` are handled by ] and ']. The operands
	;       are swapped in case of `<` and `>=` in order to achieve the correct operation. Look at `LP_BOOLEAN_LESS_THAN` case
	;       in `tmpl_print_expressions` for more details.
	;   - Since all `table.*` operands are treated as strings the operands are swapped in case of `<` and `>=` operations
	;       irrespective of whether the column type is a string or not. This function handles non-string type comparison by
	;       swapping the operands back before performing a comparison.
	;   - Additionally, for all types, a comparison involving $ZYSQLNULL values should be compared without changing the
	;       operand position as this results in incorrect output. Hence we swap the operands before performing a ZYSQLNULL
	;       comparison.
	;   - If either one of the operand is a $ZYSQLNULL then that operand is a composite NULL, return without any additional
	;       processing. $ZYSQLNULL result is expected in this case.
	QUIT:(($ZYISSQLNULL(firstOperand))!($ZYISSQLNULL(secondOperand))) $ZYSQLNULL
	NEW result,curResult,limitingCompareReached,limitingCompareResult
	SET (curResult,result)=1
	SET limitingCompareReached=0 ; Used to quit out of the FOR loop below if comparison result is found mid-way
	FOR i=1:1:numColumns DO  QUIT:(1=limitingCompareReached)
	. NEW firstCol,secondCol,firstColVal,secondColVal,type,isEquals
	. ; Get the mval piece which corresponds to the column number i
	. SET firstCol=$$mvalPiece(firstOperand,i)
	. SET secondCol=$$mvalPiece(secondOperand,i)
	. SET isEquals=0
	. ; Get the value of the mval piece i.e. value of the column
	. SET firstColVal=$$mval2str(firstCol)
	. SET secondColVal=$$mval2str(secondCol)
	. ; get i'th colTypeList value
	. SET type=$PIECE(colTypeList,",",i)
	. ; Apply ForceNumeric if the operands are non-string
	. IF ("f"=type)  DO
	. . ; Get the numeric value of the columns
	. . SET firstColVal=$$ForceNumeric(firstColVal)
	. . SET secondColVal=$$ForceNumeric(secondColVal)
	. ; Save original operand values
	. NEW tmpFirstColVal,tmpSecondColVal
	. SET tmpFirstColVal=firstColVal
	. SET tmpSecondColVal=secondColVal
	. IF ((("<"=operator)!(">="=operator))) DO
	. . ; The below swap is done because table.* are treated as strings by tmpl_print_expression and this results in the
	. . ; operands being swapped at the comparison cases in tmpl_print_expression and this swap results in incorrect comparison
	. . ; when $ZYSQLNULL and numeric data is involved. Because of that we perform a swap below. In case it turns out to be a
	. . ; pure string comparison we swap back at the string comparison code below so that the reasoning behind
	. . ; tmpl_print_expression swap still applies for this operation.
	. . NEW tmpColVal
	. . SET tmpColVal=firstColVal
	. . SET firstColVal=secondColVal
	. . SET secondColVal=tmpColVal
	. IF (($ZYISSQLNULL(firstColVal))&($ZYISSQLNULL(secondColVal))) DO
	. . ; Two NULL values are considered equal
	. . SET curResult=$SELECT(">"=operator:0,"'="=operator:0,"<="=operator:1,">="=operator:1,"<"=operator:0,"="=operator:1,1:1)
	. . ; Unconditionally set `isEquals` as it is needed only for `<`,`<=`,`>` and `>=` than operation and code at the end
	. . ; selectively ignores its value for `=` and `!=` operation.
	. . SET isEquals=1
	. ELSE  IF ($ZYISSQLNULL(firstColVal)) DO
	. . ; NULL is considered > non-NULL
	. . SET curResult=$SELECT(">"=operator:1,"'="=operator:1,"<="=operator:0,">="=operator:1,"<"=operator:0,"="=operator:0,1:1)
	. ELSE  IF $ZYISSQLNULL(secondColVal) DO
	. . ; NULL is considered > non-NULL
	. . SET curResult=$SELECT(">"=operator:0,"'="=operator:1,"<="=operator:1,">="=operator:0,"<"=operator:1,"="=operator:0,1:1)
	. ELSE  IF ((("<"=operator)!(">"=operator)!(">="=operator)!("<="=operator))&(firstColVal=secondColVal))  DO
	. . ; Both operand values are equal
	. . ; Set `curResult` to `0` in case of `<` and `>` operation and allow the loop to compare other column values by setting
	. . ;   `isEquals` to 1.
	. . SET:(("<"=operator)!(">"=operator)) curResult=0,isEquals=1
	. . ; Set `curResult` to `1` in case of `>=` and `<=` operation and allow the loop to compare other column values by
	. . ;   setting `isEquals` to 1.
	. . SET:(("<="=operator)!(">="=operator)) curResult=1,isEquals=1
	. ELSE  DO
	. . ; Non-NULL operands and they are not equal. Compare based on column types.
	. . IF "t"=type DO
	. . . ; STRING_LITERAL TYPE
	. . . ; There is a swap that is done before this IF block chain to handle $ZYSQLNULL case
	. . . ;   re-swap it so that we operate on the original passed operands.
	. . . IF (("<"=operator)!(">="=operator)) DO
	. . . . ; Re-swap only in these two operation cases as these are the only ones which require the reverse positioning of
	. . . . ;   operands.
	. . . . SET firstColVal=tmpFirstColVal
	. . . . SET secondColVal=tmpSecondColVal
	. . . IF (">"=operator) SET curResult=(firstColVal]secondColVal)
	. . . ELSE  IF ("'="=operator) SET curResult=(firstColVal'=secondColVal)
	. . . ELSE  IF ("<="=operator) SET curResult=(firstColVal']secondColVal)
	. . . ELSE  IF (">="=operator) SET curResult=(firstColVal']secondColVal)
	. . . ELSE  IF ("<"=operator) SET curResult=(firstColVal]secondColVal)
	. . . ELSE  SET curResult=(firstColVal=secondColVal) ; "="=operator
	. . ELSE  DO
	. . . ; 'f'= type. Choose operators accordingly.
	. . . IF (">"=operator) SET curResult=(firstColVal>secondColVal)
	. . . ELSE  IF ("'="=operator) SET curResult=(firstColVal'=secondColVal)
	. . . ELSE  IF ("<="=operator) SET curResult=(firstColVal<=secondColVal)
	. . . ELSE  IF (">="=operator) SET curResult=(firstColVal>=secondColVal)
	. . . ELSE  IF ("<"=operator) SET curResult=(firstColVal<secondColVal)
	. . . ELSE  SET curResult=(firstColVal=secondColVal) ; "="=operator
	. IF (("="=operator)&(0=curResult)) DO
	. . SET limitingCompareReached=1
	. . QUIT
	. ELSE  IF (("'="=operator)&(1=curResult)) DO
	. . SET limitingCompareReached=1
	. . QUIT
	. ELSE  IF (("<"=operator)!(">"=operator)!("<="=operator)!(">="=operator)) DO
	. . QUIT:(1=isEquals)  ; Both values are equal, QUIT right here so that next column values get compared
	. . ; We only reach the following code if the operands are not equal
	. . ;  so exit the loop as we found our comparison result.
	. . SET limitingCompareReached=1
	. . QUIT
	SET result=curResult
	QUIT result

; -------------------------------------------------------------------------------------------------------------------
; A STRING type column value 'abcd' would be stored as the subscript '#abcd' in the ^ydbAIM.* global (this is to
; ensure ordering of strings that contain canonical numbers stays correct.
; Below 2 helper functions are used to convert from/to STRING type column value to/from subscript value in ^ydbAIM.*
; cross reference global. See YDBAIM#73 (and YDBOcto#616) for more details.

strcolval2aimsubs(colvalue)
	QUIT "#"_colvalue

aimsubs2strcolval(subscript)
	QUIT $ZEXTRACT(subscript,2,$ZLENGTH(subscript))

; -------------------------------------------------------------------------------------------------------------------

