/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

/* Define _XOPEN_SOURCE to prevent the following two compiler warnings:
 *	warning: implicit declaration of function 'strptime'; did you mean 'strftime'? [-Wimplicit-function-declaration]
 *	warning: assignment to 'char *' from 'int' makes pointer from integer without a cast [-Wint-conversion]
 */
#define _XOPEN_SOURCE
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <assert.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"

#define DATE_SUFFIX_LEN	 2
#define DEFAULT_DATE_LEN 20 // e.g. 2009-10-04 22:23:00, plus null terminator

/* Used to get current year from struct tm.tm_year. This is needed because this
 * field gives the difference between the current year and the year 1900.
 *
 * For more information see the manual page: `man ctime`.
 */
#define YEAR_OFFSET 1900

#define MAX_YEAR	  9999
#define MONTHS_IN_YEAR	  12
#define DAYS_IN_YEAR	  365
#define DAYS_IN_MONTH	  31
#define DAYS_IN_WEEK	  7
#define HOURS_IN_DAY	  24
#define MINUTES_IN_HOUR	  60
#define SECONDS_IN_MINUTE 60

#define TIME_FORMAT_LEN_2  2
#define TIME_FORMAT_LEN_3  3
#define TIME_FORMAT_LEN_4  4
#define TIME_FORMAT_LEN_6  6
#define TIME_FORMAT_LEN_8  8
#define TIME_FORMAT_LEN_9  9
#define TIME_FORMAT_LEN_10 10
#define TIME_FORMAT_LEN_11 11

#define FREE_YDB_STRING_T(STRING)                            \
	{                                                    \
		if (NULL != (STRING)) {                      \
			if (NULL != (STRING)->address) {     \
				ydb_free((STRING)->address); \
			}                                    \
			ydb_free((STRING));                  \
		}                                            \
	}

#define PRINT_WEEK(BUFFER, INDEX, WEEK)                                                                                           \
	{                                                                                                                         \
		int copied, max;                                                                                                  \
                                                                                                                                  \
		max = TIME_FORMAT_LEN_10; /* Normally, two digits plus null terminator. However, in cases where a 0 month or date \
					     is given, */                                                                         \
		/* WEEK can be as high as 613566752, as a result of the calculations used by MySQL, which are */                  \
		/* replicated in the `get_week()` function defined below. Since this macro aims to mirror MySQL */                \
		/* behavior, we accept this otherwise senseless value, which has 9 digits instead of the expected */              \
		/* maximum of 2. In any case, counting only the digits of the given week number leads to truncation */            \
		/* by snprintf due to the inclusion of the null terminator, so increment the maximum number of */                 \
		/* digits by 1. */                                                                                                \
		if (10 > WEEK) {                                                                                                  \
			/* Add leading zero for single digit week numbers */                                                      \
			copied = snprintf(&BUFFER[INDEX], max, "0%d", WEEK);                                                      \
		} else {                                                                                                          \
			copied = snprintf(&BUFFER[INDEX], max, "%d", WEEK);                                                       \
		}                                                                                                                 \
		assert(copied < max); /* Ignore null terminator */                                                                \
		INDEX += copied;                                                                                                  \
	}

#define RETURN_IF_DATE_FIELD_TOO_LONG(I, MAX) \
	{                                     \
		if (I >= MAX) {               \
			return 1;             \
		}                             \
	}

#define PROCESS_NONALPHABETIC_DELIMITER(CHAR)                                                                                  \
	{                                                                                                                      \
		/* Multiple non-numeric characters may be used to delimit fields, so consume any and all such characters */    \
		/* before attempting to read the next field. However, alphabetic characters are *not* accepted by MySQL for */ \
		/* delimiting year, month, and day fields. So, in that case, return 1 to signal an error. */                   \
		while (!(('0' <= *(CHAR)) && ('9' >= *(CHAR))) && ('\0' != *(CHAR))) {                                         \
			if ((('A' <= *(CHAR)) && ('Z' >= *(CHAR))) || (('a' <= *(CHAR)) && ('z' >= *(CHAR)))) {                \
				return 1;                                                                                      \
			}                                                                                                      \
			(CHAR)++;                                                                                              \
		}                                                                                                              \
	}

#define PROCESS_ALPHABETIC_DELIMITER(CHAR)                                                                                         \
	{                                                                                                                          \
		while (!(('0' <= *(CHAR)) && ('9' >= *(CHAR))) && ('\0' != *(CHAR))) {                                             \
			if ((('A' <= *(CHAR)) && ('Z' >= *(CHAR))) || (('a' <= *(CHAR)) && ('z' >= *(CHAR)))) {                    \
				/* Set the current character to the null terminator to cease date string processing, and signal */ \
				/* to the caller to set all remaining fields to 0. */                                              \
				*(CHAR) = '\0';                                                                                    \
				break;                                                                                             \
			}                                                                                                          \
			(CHAR)++;                                                                                                  \
		}                                                                                                                  \
	}

#define VALUE_OUT_OF_RANGE(VALUE) (((LONG_MIN == VALUE) || (LONG_MAX == VALUE)) && (ERANGE == errno))

/* MySQL WEEK modes:
 *
 * Mode		First day of week 	Range 	Week 1 is the first week...
 * 0		Sunday			0-53 	...with a Sunday in this year
 * 1		Monday			0-53 	...with 4 or more days this year
 * 2		Sunday			1-53 	...with a Sunday in this year
 * 3		Monday			1-53 	...with 4 or more days this year
 * 4		Sunday			0-53 	...with 4 or more days this year
 * 5		Monday			0-53 	...with a Monday in this year
 * 6		Sunday			1-53 	...with 4 or more days this year
 * 7		Monday			1-53 	...with a Monday in this year
 *
 * Note that "week" in the heading of column 2 above does not mean the same thing as "week" in column 4. Rather, the
 * "First day of week" noted in the heading of column 2 means the first day of the *calendar* week. In contrast, the "week"
 * referenced in column 4 means "seven-day period" and does not necessarily coincide with the calendar week. Accordingly, it is
 * possible that week 1, as determined by reference to column 4, starts with *neither* Sunday or Monday.
 *
 * As a result, the %v, %V, %u, and %U formatting cases below are implicated, as they represent week numbers. Specifically, each of
 * these represents a "week" in the sense of "seven-day period", such that each "week" may not begin with either Sunday or Monday,
 * regardless of the WEEK mode.
 *
 * Source: https://dev.mysql.com/doc/refman/8.0/en/date-and-time-functions.html#function_week
 */

// Days of the week in numeric representation, indexed from 0
typedef enum DayOfWeek { SUNDAY = 0, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY } DayOfWeek;

// Months of the year in numeric representation, indexed from 0
typedef enum MonthOfYear {
	JANUARY = 0,
	FEBRUARY,
	MARCH,
	APRIL,
	MAY,
	JUNE,
	JULY,
	AUGUST,
	SEPTEMBER,
	OCTOBER,
	NOVEMBER,
	DECEMBER
} MonthOfYear;

typedef enum LastWeek {
	WEEK_MAX = 53,	 /* Maximum week number in a year, i.e. the last week of the year is
			  * split with the first week of the following year.
			  */
	WEEK_MINUS = -1, /* Special case used to signal that the given week is the last week of the previous year, and so can be
			  * subtracted from WEEK_MAX. This value may be used to adjust the week or year number accordingly
			  * by any caller receiving this as a return value, e.g. setting the year to year - 1.
			  */
} LastWeek;

// Range of week numbers: either 0-53 or 1-53. Varies depending on WEEK() mode
typedef enum WeekRange {
	WEEK_ZERO,
	WEEK_ONE,
} WeekRange;

int twenty_four_to_twelve_hour(int hour) {
	assert(0 <= hour);
	if (0 == hour) {
		return 12;
	} else if (12 >= hour) {
		return hour;
	} else {
		assert(24 > hour);
		return hour - 12;
	}
}

/* Calculate the day number of the year for a given date. This is necessary for correctly determining week numbers depending on the
 * MySQL WEEK() mode. Since there are more MySQL `WEEK()` modes than week formats supported by `strftime()` we cannot push this
 * calculation to that function, but must do it ourselves to ensure correct results across all MySQL `WEEK()` modes.
 *
 * Since no standard library function was found for this use case, these calculations were derived from the MySQL `calc_daynr()`
 * function defined in:
 *	https://github.com/mysql/mysql-server/blob/8.0/mysys/my_time.cc
 *
 *
 */
long get_day_num(struct tm *tm) {
	long offset;
	int  temp;
	int  year, month, day;

	year = tm->tm_year + YEAR_OFFSET;
	month = tm->tm_mon;
	day = tm->tm_mday;

	offset = (DAYS_IN_YEAR * year) + (DAYS_IN_MONTH * month) + day;
	if (FEBRUARY >= month) {
		year--;
	} else {
		offset -= (((month + 1) * 4) + 23) / 10; // Month + 1 to index from 1 instead of 0
	}
	temp = (((year / 100) + 1) * 3) / 4;

	return (offset + (year / 4) - temp);
}

/* Determine which day of the week the given date falls on.
 *
 * These calculations were derived from the MySQL `calc_weekday()` function defined in:
 *	https://github.com/mysql/mysql-server/blob/8.0/mysys/my_time.cc
 */
unsigned int get_weekday_from_day_num(unsigned int day_num, DayOfWeek first_weekday) {
	return (day_num + (5 + ((SUNDAY == first_weekday) ? 1 : 0))) % DAYS_IN_WEEK;
}

/* Read through a string representing a date, and use the date information
 * to populate a tm struct for later processing and populate `out` with a new date string
 * using the `YYYY-MM-DD hh:mm:ss` format for possible later use with `strptime()`.
 *
 * DATE_FORMAT() also accepts dates in the following forms, which must be checked for:
 *   1. Date alone, with the time omitted
 *   2. A month value of 00
 *   3. A day value of 00
 *   4. A year value of 0000
 *   5. A combination of the above
 *
 * If the date is any other form, it is invalid. In that case, return 1. Otherwise, return 0.
 */
int get_date_from_string(char *in, char *out, ssize_t out_len, struct tm *tm) {
	char *c;
	char  buffer[INT32_TO_STRING_MAX + 1];
	long  temp_long;
	int   i, copied;

	c = in;
	// Get year
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (MAX_YEAR >= temp_long)) {
			tm->tm_year = (int)temp_long - YEAR_OFFSET;
		} else {
			return 1;
		}
		if ('-' != *c) {
			return 1;
		} else {
			PROCESS_NONALPHABETIC_DELIMITER(c);
		}
	}
	// Get month
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (MONTHS_IN_YEAR >= temp_long)) {
			tm->tm_mon = (int)temp_long;
			tm->tm_mon--; // Change from 1-indexed to 0-indexed, per `man strptime`
		} else {
			return 1;
		}
		if ('-' != *c) {
			return 1;
		} else {
			PROCESS_NONALPHABETIC_DELIMITER(c);
		}
	}
	// Get day
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (DAYS_IN_MONTH >= temp_long)) {
			tm->tm_mday = (int)temp_long;
		} else {
			return 1;
		}
	}

	PROCESS_ALPHABETIC_DELIMITER(c);
	if ('\0' == *c) {
		/* There is an alphabetic character in the delimiter, or there are no more fields to be read in the date string.
		 * So, set all remaining time fields to 0 to mimic MySQL behavior.
		 */
		tm->tm_hour = 0;
		tm->tm_min = 0;
		tm->tm_sec = 0;
	}

	// Get hour
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (HOURS_IN_DAY > temp_long)) {
			tm->tm_hour = (int)temp_long;
		} else {
			return 1;
		}
		PROCESS_ALPHABETIC_DELIMITER(c);
		if ('\0' == *c) {
			/* There is an alphabetic character in the delimiter, or there are no more fields to be read in the date
			 * string. So, set all remaining time fields to 0 to mimic MySQL behavior.
			 */
			tm->tm_min = 0;
			tm->tm_sec = 0;
		}
	}
	// Get minute
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (MINUTES_IN_HOUR > temp_long)) {
			tm->tm_min = (int)temp_long;
		} else {
			return 1;
		}
		PROCESS_ALPHABETIC_DELIMITER(c);
		if ('\0' == *c) {
			/* There is an alphabetic character in the delimiter, or there are no more fields to be read in the date
			 * string. So, set all remaining time fields to 0 to mimic MySQL behavior.
			 */
			tm->tm_sec = 0;
		}
	}
	// Get second
	if ('\0' != *c) {
		for (i = 0; ((('0' <= *c) && ('9' >= *c)) && ('\0' != *c)); i++, c++) {
			RETURN_IF_DATE_FIELD_TOO_LONG(i, INT32_TO_STRING_MAX);
			buffer[i] = *c;
		}
		assert(sizeof(buffer) > (size_t)i);
		buffer[i] = '\0';
		temp_long = strtol(buffer, NULL, 10);
		if (!VALUE_OUT_OF_RANGE(temp_long) && (0 <= temp_long) && (SECONDS_IN_MINUTE > temp_long)) {
			tm->tm_sec = (int)temp_long;
		} else {
			return 1;
		}
	}

	/* Use the date information gleaned from the input string to populate a new date string using the `YYYY-MM-DD hh:mm:ss`
	 * format for possible later use with `strptime()`.
	 */
	copied = snprintf(out, out_len, "%d-%d-%d %d:%d:%d", tm->tm_year + YEAR_OFFSET, tm->tm_mon + 1, tm->tm_mday, tm->tm_hour,
			  tm->tm_min, tm->tm_sec);
	assert(copied < out_len);
	UNUSED(copied);

	return 0;
}

/* Determine whether the given year is a leap year , based on the Gregorian leap year algorithm. Sources:
 *	https://en.wikipedia.org/wiki/Leap_year#Algorithm
 *	https://stackoverflow.com/questions/41068969/calculate-total-number-of-days-in-a-year#41069058
 */
boolean_t is_leap(unsigned int year) {
	boolean_t is_leap, not_centennial, is_quad_centennial;

	is_leap = ((year & 3) == 0);
	not_centennial = (year % 100 > 0);
	is_quad_centennial = (year % 400 == 0);

	return ((is_leap && not_centennial) || is_quad_centennial);
}

/* Determine the total number of days in the given year, based on the
 * Gregorian leap year algorithm.
 *
 */
unsigned int get_days_in_year(unsigned int year) { return ((is_leap(year)) ? 366 : 365); }

/* Determine which week of the year the given date falls in. Note that no standard library function exists for these calculations,
 * as MySQL `WEEK()` modes are specific to MySQL and do not conform to any known standards.
 *
 * These calculations were derived from the MySQL `calc_week()` function defined in:
 *	https://github.com/mysql/mysql-server/blob/8.0/mysys/my_time.cc
 */
int get_week(struct tm *tm, unsigned int *year, DayOfWeek first_weekday, WeekRange start_week, boolean_t use_ISO) {
	struct tm    temp_tm;
	unsigned int days, weekday;
	long	     day_num, first_day_num;

	/* Manually, partially populate a temporary tm struct with the first day of the year
	 * for use in retrieval of the day number of the first day of the year.
	 */
	temp_tm.tm_year = tm->tm_year;
	temp_tm.tm_mon = JANUARY;
	temp_tm.tm_mday = 1; // i.e. the 1st of the month

	first_day_num = get_day_num(&temp_tm);
	day_num = get_day_num(tm);
	weekday = get_weekday_from_day_num(first_day_num, first_weekday);
	*year = tm->tm_year + YEAR_OFFSET;

	if (JANUARY > tm->tm_mon) {
		/* The month specified in the original date string was 0, which is invalid and signalled by a tm->tm_mon value of
		 * -1. This breaks the following calculations However, MySQL accepts this case and produces potentially incorrect
		 * output rather than returning an error. So, bump the month to 0 (January) and proceed.
		 */
		// tm->tm_mon = JANUARY;
	}
	if ((JANUARY == tm->tm_mon) && ((unsigned int)tm->tm_mday <= (DAYS_IN_WEEK - weekday))) {
		if ((WEEK_ZERO == start_week) && ((!use_ISO && (SUNDAY != weekday)) || (use_ISO && (THURSDAY <= weekday)))) {
			return WEEK_ZERO;
		}
		start_week = WEEK_ONE;

		(*year)--;
		days = get_days_in_year(*year);
		first_day_num -= days;
		weekday = (weekday + WEEK_MAX * DAYS_IN_WEEK - days) % DAYS_IN_WEEK;
	}

	if ((!use_ISO && (SUNDAY != weekday)) || (use_ISO && (THURSDAY <= weekday))) {
		days = day_num - (first_day_num + (DAYS_IN_WEEK - weekday));
	} else {
		days = day_num - (first_day_num - weekday);
	}

	if ((WEEK_ONE == start_week) && (days >= ((WEEK_MAX + WEEK_MINUS) * DAYS_IN_WEEK))) {
		weekday = (weekday + get_days_in_year(*year)) % DAYS_IN_WEEK;
		if ((use_ISO && (THURSDAY > weekday)) || (!use_ISO && (SUNDAY == weekday))) {
			(*year)++;
			return WEEK_ONE;
		}
	}

	return (days / DAYS_IN_WEEK) + 1;
}

/* Calculate the maximum possible length of the specified date/time format string based on the formatting symbols contained within
 * it. This will allow us to guarantee allocation of a sufficiently large buffer to store the final date/time string, with all
 * fields populated.
 *
 * In: ydb_string_t* containing the date/time format string
 * Out: The maximum possible length of the input string when fully populated
 */
size_t get_max_date_length(ydb_string_t *time_format) {
	char * time_format_str;
	size_t format_len, time_format_len, i;

	format_len = 0;
	time_format_str = time_format->address;
	time_format_len = time_format->length;
	for (i = 0; i < time_format_len; i++) {
		if (('%' == time_format_str[i]) && ((i + 1) != time_format_len)) {
			switch (time_format_str[i + 1]) {
			case 'd':
				// %d: Day of the month, numeric (00..31)
			case 'H':
				// %H: Hour (00..23)
			case 'h':
				// %h: Hour (01..12)
			case 'I':
				// %I: Hour (01..12)
			case 'i':
				// %i: Minutes, numeric (00..59)
			case 'k':
				// %k: Hour (0..23)
			case 'l':
				// %l: Hour (1..12)
			case 'p':
				// %p: AM or PM
			case 'S':
				// %S: Seconds (00..59)
			case 's':
				// %s: Seconds (00..59)
			case 'm':
				// %m: Month, numeric (00..12)
			case 'w':
				// %w: Day of the week (0=Sunday..6=Saturday)
			case 'y':
				// %y: Year, numeric (two digits)
			case 'e':
				// %e: Day of the month, numeric (0..31) [strip leading 0]
			case 'c':
				// %c: Month, numeric (0..12) [strip leading 0]
				format_len += TIME_FORMAT_LEN_2;
				break;
			case 'a':
				// %a: Abbreviated weekday name (Sun..Sat)
			case 'b':
				// %b: Abbreviated month name (Jan..Dec)
			case 'j':
				// %j: Day of year (001..366)
				format_len += TIME_FORMAT_LEN_3;
				break;
			case 'Y':
				// %Y: Year, numeric, four digits
			case 'D':
				// %D: Day of the month with English suffix (0th, 1st, 2nd, 3rd, ...)
			case 'X':
				// %X: Year for the week where Sunday is the first day of the week, numeric, four digits
			case 'x':
				// %x: Year for the week, where Monday is the first day of the week, numeric, four digits
				format_len += TIME_FORMAT_LEN_4;
				break;
			case 'U':
				// %U: Week (00..53), where Sunday is the first day of the week; WEEK() mode 0
			case 'u':
				// %u: Week (00..53), where Monday is the first day of the week; WEEK() mode 1
			case 'V':
				// %V: Week (01..53), where Sunday is the first day of the week; WEEK() mode 2
			case 'v':
				// %v: Week (01..53), where Monday is the first day of the week; WEEK() mode 3
				format_len += TIME_FORMAT_LEN_10; // Per comment in PRINT_WEEK macro, the week number may be as high
								  // as 613566752
				break;
			case 'f':
				// %f: Microseconds (000000..999999)
				format_len += TIME_FORMAT_LEN_6;
				break;
			case 'T':
				// %T: Time, 24-hour (hh:mm:ss)
				format_len += TIME_FORMAT_LEN_8;
				break;
			case 'M':
				// %M: Month name (January..December)
			case 'W':
				// %W: Weekday name (Sunday..Saturday)
				format_len += TIME_FORMAT_LEN_9; // strlen("Wednesday") == 9
				break;
			case 'r':
				// %r: Time, 12-hour (hh:mm:ss followed by AM or PM, e.g. "hh:mm:ss AM")
				format_len += TIME_FORMAT_LEN_11;
				break;
			case '%':
			default:
				/* No action needed, i.e.:
				 *   %%: A literal % character
				 *   %x: x, for any "x" not listed above, i.e. no expansion, just remove '%'
				 */
				format_len++;
				break;
			}
		} else {
			format_len++;
		}
	}
	format_len++; // Null terminator

	return format_len;
}

/* Populate the format fields of a SQL date/time format string that
 * are not valid format symbols for strftime.
 *
 * In:	A character string containing the SQL time format string to be populated
 * Out:	A newly allocated character string containing a populated SQL time format string. Note that only the special cases listed
 *	above will be populated. All format symbols that can be handled via strftime will be left intact for later processing by
 *	that function.
 */
ydb_string_t *populate_sql_time_format(ydb_string_t *time_format, struct tm *tm, boolean_t *call_strftime) {
	ydb_string_t *ret;
	char *	      time_format_str;
	size_t	      format_len, time_format_len, i, j, max, copied;
	unsigned int  year;
	int	      week;

	format_len = get_max_date_length(time_format);
	/* These allocations may be freed not in Octo but by YottaDB after the external call returns, depending on the value of
	 * `call_strftime`. If `call_strftime` is TRUE, then this will be freed at the end of `ydboctoDateFormatC()`. Otherwise, it
	 * will be returned to the M caller and subsequently freed by YottaDB.
	 */
	ret = ydb_malloc(sizeof(ydb_string_t));
	ret->address = ydb_malloc(sizeof(char) * (format_len + 1)); // Null terminator
	ret->length = 0;

	assert(FALSE == *call_strftime);
	time_format_str = time_format->address;
	time_format_len = time_format->length;
	for (i = 0, j = 0; i < time_format_len; i++) {
		if (('%' == time_format_str[i]) && ((i + 1) != time_format_len)) {
			i++;
			switch (time_format_str[i]) {
			case 'e':
				// %e: Day of the month, numeric (0..31)
				max = TIME_FORMAT_LEN_2 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%d", tm->tm_mday);
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'D':
				// %D: Day of the month with English suffix (0th, 1st, 2nd, 3rd, ...)
				max = TIME_FORMAT_LEN_4 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%d", tm->tm_mday);
				assert(copied < max);
				j += copied;
				// Add suffix
				if ((2 == copied) && ('1' == ret->address[j - 2])) { // i.e. 11th, 12th, etc.
					memcpy(&ret->address[j], "th", DATE_SUFFIX_LEN);
				} else {
					assert(0 < j);
					switch (ret->address[j - 1]) {
					case '1':
						memcpy(&ret->address[j], "st", DATE_SUFFIX_LEN);
						break;
					case '2':
						memcpy(&ret->address[j], "nd", DATE_SUFFIX_LEN);
						break;
					case '3':
						memcpy(&ret->address[j], "rd", DATE_SUFFIX_LEN);
						break;
					case '4':
					case '5':
					case '6':
					case '7':
					case '8':
					case '9':
					case '0':
						memcpy(&ret->address[j], "th", DATE_SUFFIX_LEN);
						break;
					default:
						assert(FALSE);
						break;
					}
				}
				j += DATE_SUFFIX_LEN;
				break;
			case 'f':
				// %f: Microseconds (000000..999999)
				max = TIME_FORMAT_LEN_6 + 1; // Null terminator
				/* It's unclear what this is supposed to do, as dates passed to DATE_FORMAT do not have microsecond
				 * resolution. So, just use 000000 as this seems to be MySQL's behavior.
				 */
				copied = snprintf(&ret->address[j], max, "000000");
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'a': // Abbreviated weekday name (Sun..Sat)
			case 'w': // Day of the week (0=Sunday..6=Saturday)
			case 'W': // %W: Weekday name (Sunday..Saturday)
				if ((0 != tm->tm_mday) && (0 <= tm->tm_mon) && (0 != tm->tm_year + YEAR_OFFSET)) {
					/* Date is acceptable to strptime() and so we already know the weekday. So, just
					 * fall-through to default case and later use strftime() with tm struct as is.
					 */
				} else {
					/* Month or day is 0, but year is not. Since strptime() doesn't support a 0 month or
					 * 0 day value we need to manually calculate the weekday value in this case before
					 * we call strftime().
					 */
					tm->tm_wday = get_weekday_from_day_num(get_day_num(tm), SUNDAY);
				}
				ret->address[j] = time_format_str[i - 1];
				j++;
				if ('W' == time_format_str[i]) {
					/* MySQL %W is equivalent to strftime's %A, so just replace the former with
					 * the latter and allow later pass through strftime to handle this case.
					 */
					ret->address[j] = 'A';
				} else {
					/* `%a` and `%w` are accepted by strftime, so just copy the format code over as is in these
					 * cases.
					 */
					ret->address[j] = time_format_str[i];
				}
				j++;
				*call_strftime = TRUE;
				break;
			case 'k': // Hour (0..23)
				/* Even though this format maps to the same strptime format code,
				 * we need to do this manually as strptime includes a space for hours < 10,
				 * where MySQL does not.
				 */
				max = TIME_FORMAT_LEN_2 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%d", tm->tm_hour);
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'l': // Hour (1..12)
				/* Even though this format maps to the same strptime format code,
				 * we need to do this manually as strptime includes a space for hours < 10,
				 * where MySQL does not.
				 */
				max = TIME_FORMAT_LEN_2 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%d", twenty_four_to_twelve_hour(tm->tm_hour));
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'Y': // Year, numeric, four digits
				/* Even though this format maps to the same strptime format code,
				 * we need to do this manually as strptime doesn't 0 pad years < 1000
				 * whereas MySQL does.
				 */
				max = TIME_FORMAT_LEN_4 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%04d", (tm->tm_year + YEAR_OFFSET));
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'X':
				// %X: Year for the week, where Sunday is the first day of the week, numeric, four digits
			case 'x':
				// %x: Year for the week, where Monday is the first day of the week, numeric, four digits
				max = TIME_FORMAT_LEN_4 + 1;						     // Null terminator
				(('X' == time_format_str[i]) ? get_week(tm, &year, SUNDAY, WEEK_ONE, FALSE)  // %X, mode 2
							     : get_week(tm, &year, MONDAY, WEEK_ONE, TRUE)); // %x, mode 3
				copied = snprintf(&ret->address[j], max, "%04d", year);
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'c':
				// %c: Month, numeric (0..12) [strip leading 0]
				max = TIME_FORMAT_LEN_2 + 1; // Null terminator
				copied = snprintf(&ret->address[j], max, "%d", tm->tm_mon + 1);
				assert(copied < max); // Ignore null terminator
				j += copied;
				break;
			case 'U':
				// Week (00..53), where Monday is the first day of the week; WEEK() mode 0

				week = get_week(tm, &year, SUNDAY, WEEK_ZERO, FALSE); // %U, mode 0
				PRINT_WEEK(ret->address, j, week);
				break;
			case 'u':
				/* Week (00..53), where Monday is the first day of the week; WEEK() mode 1
				 *
				 * MySQL %u is similar to strftime's %V, i.e. reflects ISO 8601, but treats any days falling
				 * outside of the first week with 4 days in the specified year as belonging to the previous
				 * year. This is reflected by assigning such days to week `0`.
				 */

				week = get_week(tm, &year, MONDAY, WEEK_ZERO, TRUE); // %u, mode 1
				PRINT_WEEK(ret->address, j, week);
				*call_strftime = TRUE;
				break;
			case 'V':
				/* %V: Week (01..53), where Sunday is the first day of the week; WEEK() mode 2
				 *
				 * MySQL %V is equivalent to strftime's %U, with the exception that %U's range extends to 0.
				 */

				week = get_week(tm, &year, SUNDAY, WEEK_ONE, FALSE); // %V, mode 2
				PRINT_WEEK(ret->address, j, week);
				*call_strftime = TRUE;
				break;
			case 'v':
				/* %v: Week (01..53), where Monday is the first day of the week; WEEK() mode 3
				 *
				 * MySQL %v is equivalent to strftime's %V, i.e. ISO 8601, so just replace the former with
				 * the latter and allow later pass through strftime to handle this case.
				 */

				week = get_week(tm, &year, MONDAY, WEEK_ONE, TRUE); // %v, mode 3
				PRINT_WEEK(ret->address, j, week);
				*call_strftime = TRUE;
				break;
			case 'h':
				/* Hour (01..12)
				 *
				 * MySQL %h is equivalent to strftime's %I, so just replace the former with
				 * the latter and allow later pass through strftime to handle this case.
				 */
				ret->address[j] = time_format_str[i - 1];
				j++;
				ret->address[j] = 'I';
				j++;
				*call_strftime = TRUE;
				break;
			case 'i':
				/* Minutes, numeric (00..59)
				 *
				 * MySQL %i is equivalent to strftime's %M, so just replace the former with
				 * the latter and allow later pass through strftime to handle this case.
				 */
				ret->address[j] = time_format_str[i - 1];
				j++;
				ret->address[j] = 'M';
				j++;
				*call_strftime = TRUE;
				break;
			case 'M':
				/* Month name (January..December)
				 *
				 * MySQL %M is equivalent to strftime's %B, so just replace the former with
				 * the latter and allow later pass through strftime to handle this case.
				 */
				if (-1 == tm->tm_mon) {
					/* We cannot get the name of an invalid month, so cleanup and signal to the caller to
					 * return $ZYSQLNULL by returning NULL here.
					 */
					FREE_YDB_STRING_T(ret);
					return NULL;
				}
				ret->address[j] = time_format_str[i - 1];
				j++;
				ret->address[j] = 'B';
				j++;
				*call_strftime = TRUE;
				break;
			case 's':
				/* Seconds (00..59)
				 *
				 * MySQL %s is equivalent to strftime's %S, so just replace the former with
				 * the latter and allow later pass through strftime to handle this case.
				 */
				ret->address[j] = time_format_str[i - 1];
				j++;
				ret->address[j] = 'S';
				j++;
				*call_strftime = TRUE;
				break;
			case 'b': // Abbreviated month name (Jan..Dec)
			case 'd': // Day of the month, numeric (00..31)
			case 'H': // Hour (00..23)
			case 'I': // Hour (01..12)
			case 'j': // Day of year (001..366)
			case 'p': // AM or PM
			case 'r': // Time, 12-hour (hh:mm:ss followed by AM or PM)
			case 'S': // Seconds (00..59)
			case 'T': // Time, 24-hour (hh:mm:ss)
			case 'm': // Month, numeric (00..12)
			case 'y': // Year, numeric (two digits)
			case '%': // A literal % character
				/* Each of the above is a valid strftime format, so just preserve the format for a later call to
				 * strftime.
				 *
				 * Note that the '%' case is handled as a valid `strftime()` format code and not through the default
				 * case per this GitLab discussion comment:
				 *   https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/991#note_875953381
				 */
				if (('b' == time_format_str[i]) && (-1 == tm->tm_mon)) { // Abbreviated month name (Jan..Dec)
					/* We cannot abbreviate the name of an invalid month, so cleanup and signal to the caller to
					 * return $ZYSQLNULL by returning NULL here.
					 */
					FREE_YDB_STRING_T(ret);
					return NULL;
				}

				if ('j' == time_format_str[i]) { // Day of year 001-366
					if ((0 == tm->tm_mday) || (0 >= tm->tm_mon)) {
						/* strftime() does not calculate a day of the year for month or day values of 0. So,
						 * do this calculation manually here, using the existing get_day_num() function.
						 *
						 * However, note that get_day_num() returns the number of days since 0000-00-00,
						 * NOT the day number of the year. To get the day number of the year, we must
						 * subtract the day number of the first day of the given year from the day number of
						 * the specified date.
						 */
						struct tm temp_tm;

						temp_tm.tm_year = tm->tm_year;
						temp_tm.tm_mon = JANUARY;
						temp_tm.tm_mday = 1; // i.e. the 1st of the month
						tm->tm_yday = get_day_num(tm) - get_day_num(&temp_tm);
						if ((0 > (tm->tm_yday + 1)) && (-10 < (tm->tm_yday + 1))) { // +1 explained below
							/* The day of the year is between -1 and -9. If passed to strftime, the
							 * result will be of the format e.g. -0n, where n is the absolute value of
							 * the day of the year. For example, a day of the year of `-5` will yield an
							 * output of `-05` from strftime().
							 *
							 * However, MySQL prefixes 0 to the day of the year value *before* the
							 * negative sign, e.g. a day of the year of `-5` yields an output of `0-5`
							 * from MySQL.
							 *
							 * So, to ensure consistent behavior between MySQL and Octo, we need must
							 * skip strtime() calls when the day of the year is between -1 and -9.
							 * Instead, we must manually enforce the format used by MySQL, i.e. `0-n` in
							 * such cases.
							 *
							 * Note that we add 1 to the day of the year to reflect the behavior of both
							 * MySQL and strftime(), which will output e.g. `0-5` or `-05`,
							 * respectively, for a day of the year value of `-6`.
							 */
							max = TIME_FORMAT_LEN_3 + 1; // Null terminator
							copied = snprintf(&ret->address[j], max, "0%d", tm->tm_yday + 1);
							assert(copied < max); // Ignore null terminator
							j += copied;
							break;
						}
					}
				}
				ret->address[j] = time_format_str[i - 1];
				j++;
				ret->address[j] = time_format_str[i];
				j++;
				*call_strftime = TRUE;
				break;
			default:
				// %x: x, for any "x" not listed above, i.e. no expansion, just ignore '%' and copy "x"
				ret->address[j] = time_format_str[i];
				j++;
				break;
			}
		} else {
			ret->address[j] = time_format_str[i];
			j++;
		}
	}
	ret->address[j] = '\0';
	ret->length = format_len; // Full allocation length, may be needed for later strftime call

	return ret;
}

/* Implements the MySQL DATE_FORMAT() function. If the MySQL WEEK() function is implemented (#840), some logic may be duplicated. If
 * so, duplicate logic should be abstracted into a common module.
 */
ydb_string_t *ydboctoDateFormatC(int count, ydb_string_t *date, ydb_string_t *format) {
	ydb_string_t *intermediate_format, *ret;
	char *	      date_str, *result;
	char	      revised_date[DEFAULT_DATE_LEN];
	struct tm     tm;
	int	      status;
	boolean_t     call_strftime;

	// 0-initialize fields of tm struct
	memset(&tm, 0, sizeof(struct tm));
	UNUSED(count);

	/* Null terminate date string for use in strptime. This is needed
	 * since ydb_string_ts are not guaranteed to be null terminated.
	 */
	date_str = ydb_malloc(date->length + 1); // Null terminator
	memcpy(date_str, date->address, date->length);
	date_str[date->length] = '\0';
	/* Parse the date string passed to DATE_FORMAT into a tm struct and populate `revised_date` with
	 * the `YYYY-MM-DD hh:mm:ss` format to ensure a single date string format for use with `strptime()`.
	 *
	 * This is necessary since DATE_FORMAT will accept multiple formats. Since it would be necessary to parse the date string to
	 * determine the correct format for `strptime()` anyway, it is simpler to just parse the string into a single format and
	 * have one `strptime()` call instead of one per accepted format.
	 */
	status = get_date_from_string(date_str, revised_date, DEFAULT_DATE_LEN, &tm);
	ydb_free(date_str);

	call_strftime = FALSE;
	if (status) {
		intermediate_format = NULL;
	} else {
		if ((0 == tm.tm_mday) && (-1 == tm.tm_mon) && (-1900 == tm.tm_year)) {
			/* A totally invalid date (0000-00-00), signal this by setting the format to NULL and handle below.
			 *
			 * Note that if tm.tm_mon == 0 then the month is January, but a value of -1 means the month is 0, which is
			 * invalid. Similarly, a tm.tm_year value of -1900 reflects of a year of 0, due to the year offset used by
			 * the tm struct.
			 */
			intermediate_format = NULL;
		} else {
			boolean_t day_valid;

			day_valid = TRUE;
			switch (tm.tm_mon) {
			case (JANUARY - 1):
				/* MySQL accepts a month of 0, i.e. JANUARY (0) - 1. See this GitLab discussion for more background:
				 *   https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/991#note_896728387
				 */
			case JANUARY:
			case MARCH:
			case MAY:
			case JULY:
			case AUGUST:
			case OCTOBER:
			case DECEMBER:
				if (DAYS_IN_MONTH < tm.tm_mday) { // 31 days per month
					intermediate_format = NULL;
					day_valid = FALSE;
				}
				break;
			case APRIL:
			case JUNE:
			case SEPTEMBER:
			case NOVEMBER:
				if ((DAYS_IN_MONTH - 1) < tm.tm_mday) { // 30 days per month
					intermediate_format = NULL;
					day_valid = FALSE;
				}
				break;
			case FEBRUARY:
				if (is_leap(tm.tm_year + YEAR_OFFSET)) {	// Leap year
					if ((DAYS_IN_MONTH - 2) < tm.tm_mday) { // 29 days per month
						intermediate_format = NULL;
						day_valid = FALSE;
					}
				} else {
					if ((DAYS_IN_MONTH - 3) < tm.tm_mday) { // 28 days per month
						intermediate_format = NULL;
						day_valid = FALSE;
					}
				}
				break;
			default:
				intermediate_format = NULL;
				day_valid = FALSE;
				break;
			}
			if (day_valid) {
				status = YDB_OK;
				if ((0 != tm.tm_mday) && (0 <= tm.tm_mon) && (0 != tm.tm_year + YEAR_OFFSET)) {
					result = strptime(revised_date, "%Y-%m-%d %T", &tm);
					if (NULL == result) {
						intermediate_format = NULL;
						status = !YDB_OK;
					}
				}
				if (YDB_OK == status) {
					intermediate_format = populate_sql_time_format(format, &tm, &call_strftime);
				}
			}
		}
	}

	if ((NULL == intermediate_format) || call_strftime) {
		// This allocation will be freed not in Octo but by YottaDB after the external call returns.
		ret = ydb_malloc(sizeof(ydb_string_t));
		if (NULL == intermediate_format) {
			/* Signal to the caller to return $ZYSQLNULL by returning an empty string. This is safe, since MySQL returns
			 * NULL if a format string of "" is passed to DATE_FORMAT(), as well as when in other edge cases that aren't
			 * reported as errors.
			 */
			// This allocation will be freed not in Octo but by YottaDB after the external call returns.
			ret->address = ydb_malloc(sizeof(char)); // Null terminator only, for empty string
			ret->address[0] = '\0';
			ret->length = 0;
		} else {
			// Populate remaining formats
			assert(call_strftime);
			// This allocation will be freed not in Octo but by YottaDB after the external call returns.
			ret->address = ydb_malloc(sizeof(char) * (intermediate_format->length)); // Null terminator included already
			ret->length = strftime(ret->address, intermediate_format->length, intermediate_format->address, &tm);
			assert(NULL != intermediate_format->address);
			FREE_YDB_STRING_T(intermediate_format);
		}
	} else {
		assert(NULL != intermediate_format);
		ret = intermediate_format;
	}

	return ret;
}
