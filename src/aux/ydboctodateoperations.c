/****************************************************************
 *								*
 * Copyright (c) 2023-2025 YottaDB LLC and/or its subsidiaries.	*
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
 *
 * Also, to be able to get `strdup()`, per the man page, _XOPEN_SOURCE needs to be 500 or above.
 */
#define _XOPEN_SOURCE 500
/* Define _POSIX_C_SOURCE to prevent the following two compiler warnings:
 * 	warning: implicit declaration of function `setenv`; did you mean `getenv`? [-Wimplicit-function-declaration]
 *	warning: implicit declaration of function `unsetenv`; did you mean `getenv`? [-Wimplicit-function-declaration]
 */
#define _POSIX_C_SOURCE 200112L
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"

// Helper macros for date/time value validation
#define ATOI(PTR, RET)                            \
	{                                         \
		long val = strtol(PTR, NULL, 10); \
		/* validate return */             \
		if (ERANGE == errno) {            \
			return 1;                 \
		}                                 \
		RET = (int)val;                   \
	}
#define IS_END(VAL)	   ('\0' == *(VAL))
#define IS_GMT_OFFSET(VAL) (('+' == *(VAL)) || ('-' == *(VAL)))
#define IS_SEC_END(VAL)	   (('.' == *(VAL)) || IS_END(VAL) || (IS_GMT_OFFSET(VAL)))
#define IS_NUMBER(VAL)	   (('0' <= *(VAL)) && ('9' >= *(VAL)))
#define IS_TIME_ZONE(LIT_C)                                                                           \
	{                                                                                             \
		if (IS_END(LIT_C)) {                                                                  \
		} else if (IS_GMT_OFFSET(LIT_C)) {                                                    \
			LIT_C++;                                                                      \
			int i;                                                                        \
			for (i = 0; i < 8; i++) {                                                     \
				/* HH or HH:MM or HH:MM:SS */                                         \
				if ((2 == i) || (5 == i)) {                                           \
					if (IS_END(LIT_C) || (' ' == *LIT_C)) {                       \
						break;                                                \
					} else if (':' == *(LIT_C)) {                                 \
						/* Valid */                                           \
					} else {                                                      \
						return 1;                                             \
					}                                                             \
				} else if (!IS_NUMBER(LIT_C)) {                                       \
					return 1;                                                     \
				}                                                                     \
				if (1 == i) {                                                         \
					/* -15:59 +15:59 */                                           \
					if ('1' < *(LIT_C - 1)) {                                     \
						return 1;                                             \
					}                                                             \
					if (('1' == *(LIT_C - 1)) && ('5' < *(LIT_C))) {              \
						/* -15:59 and +15:59 is the limiting value */         \
						return 1;                                             \
					}                                                             \
				}                                                                     \
				if (4 == i) {                                                         \
					if ('5' < *(LIT_C - 1)) {                                     \
						return 1;                                             \
					}                                                             \
					if (('1' == *(LIT_C - 4)) && ('5' == *(LIT_C - 3))) {         \
						if ('5' < *(LIT_C - 1)) {                             \
							/* -15:59 and +15:59 is the limiting value */ \
							return 1;                                     \
						}                                                     \
					}                                                             \
				}                                                                     \
				if (7 == i) {                                                         \
					/* -15:59:59 and +15:59:59 is the limiting value */           \
					if ('5' < *(LIT_C - 1)) {                                     \
						return 1;                                             \
					}                                                             \
				}                                                                     \
				LIT_C++;                                                              \
			}                                                                             \
		} else {                                                                              \
			return 1;                                                                     \
		}                                                                                     \
	}
/* For a month to be valid it needs to be between 00 and 12 or when single digit 1 and 9.
 * This macro also shifts LIT_C by the number of characters processed.
 * On invalid input this macro returns with -1.
 */
#define IS_MONTH_VALID(LIT_C)                                                                              \
	{                                                                                                  \
		const char *c = LIT_C;                                                                     \
		if (!IS_END(c) && IS_NUMBER(c)) {                                                          \
			if (!IS_END(c + 1) && IS_NUMBER(c + 1)) {                                          \
				/* 2 digit value */                                                        \
				boolean_t first_digit_is_zero = FALSE;                                     \
				if (('0' == *c) || ('1' == *c)) {                                          \
					/* Valid */                                                        \
					if ('0' == *c) {                                                   \
						first_digit_is_zero = TRUE;                                \
					}                                                                  \
				} else {                                                                   \
					/* In-valid */                                                     \
					return 1;                                                          \
				}                                                                          \
				if (first_digit_is_zero) {                                                 \
					if ('0' == *(c + 1)) {                                             \
						/* Month is 00 which is invalid */                         \
						return 1;                                                  \
					} else if (('1' <= *(c + 1)) && ('9' >= *(c + 1))) {               \
						/* Valid */                                                \
					}                                                                  \
				} else {                                                                   \
					if (('0' == *(c + 1)) || ('1' == *(c + 1)) || ('2' == *(c + 1))) { \
						/* Valid */                                                \
					} else {                                                           \
						/* Invalid */                                              \
						return 1;                                                  \
					}                                                                  \
				}                                                                          \
				LIT_C++;                                                                   \
				LIT_C++;                                                                   \
			} else {                                                                           \
				/* 1 digit value */                                                        \
				if (('1' <= *c) && ('9' >= *c)) {                                          \
					/* Valid */                                                        \
				} else {                                                                   \
					assert('0' == *c);                                                 \
					return 1;                                                          \
				}                                                                          \
				LIT_C++;                                                                   \
			}                                                                                  \
		} else {                                                                                   \
			return 1;                                                                          \
		}                                                                                          \
	}

#define IS_DATE_VALID(LIT_C)                                                                                                      \
	{                                                                                                                         \
		const char *c = LIT_C;                                                                                            \
		if (!IS_END(c) && IS_NUMBER(c)) {                                                                                 \
			if (!IS_END(c + 1) && IS_NUMBER(c + 1)) {                                                                 \
				/* 2 digit value */                                                                               \
				/* 00-31 */                                                                                       \
				boolean_t first_digit_is_zero = FALSE, first_digit_is_one_or_two = FALSE,                         \
					  first_digit_is_three = FALSE;                                                           \
				if (('0' <= *c) && ('3' >= *c)) {                                                                 \
					if ('0' == *c) {                                                                          \
						first_digit_is_zero = TRUE;                                                       \
					} else if (('1' == *c) || ('2' == *c)) {                                                  \
						first_digit_is_one_or_two = TRUE;                                                 \
					} else {                                                                                  \
						assert('3' == *c);                                                                \
						first_digit_is_three = TRUE;                                                      \
					}                                                                                         \
				} else {                                                                                          \
					return 1;                                                                                 \
				}                                                                                                 \
				if (first_digit_is_zero) {                                                                        \
					/* 01-09 */                                                                               \
					if ('0' == *(c + 1)) {                                                                    \
						return 1;                                                                         \
					}                                                                                         \
				} else if (first_digit_is_one_or_two) {                                                           \
					/* 10-19, 20-29 */                                                                        \
					/* The second digit is in valid range, it was confirmed by IS_NUMBER(c+1) call previously \
					 */                                                                                       \
				} else if (first_digit_is_three) {                                                                \
					/* 30-31 */                                                                               \
					if (('0' == *(c + 1)) || ('1' == *(c + 1))) {                                             \
						/* Valid*/                                                                        \
					} else {                                                                                  \
						return 1;                                                                         \
					}                                                                                         \
				} else {                                                                                          \
					assert(FALSE);                                                                            \
				}                                                                                                 \
				LIT_C++;                                                                                          \
				LIT_C++;                                                                                          \
			} else {                                                                                                  \
				/* 1 digit value */                                                                               \
				/* 1-9 */                                                                                         \
				if (('1' <= *c) && ('9' >= *c)) {                                                                 \
					/* Valid */                                                                               \
				} else {                                                                                          \
					assert('0' == *c);                                                                        \
					return 1;                                                                                 \
				}                                                                                                 \
				LIT_C++;                                                                                          \
			}                                                                                                         \
		} else {                                                                                                          \
			return 1;                                                                                                 \
		}                                                                                                                 \
	}
#define IS_YEAR_VALID(LIT_C)                                                                              \
	{                                                                                                 \
		const char *c = LIT_C;                                                                    \
		/* 0000 - 9999 */                                                                         \
		if (IS_END(c) || IS_END(c + 1) || IS_END(c + 2) || IS_END(c + 3)) {                       \
			return 1;                                                                         \
		} else if (!(IS_NUMBER(c) && IS_NUMBER(c + 1) && IS_NUMBER(c + 2) && IS_NUMBER(c + 3))) { \
			return 1;                                                                         \
		} else if (('0' == *c) && ('0' == *(c + 1)) && ('0' == *(c + 2)) && ('0' == *(c + 3))) {  \
			return 1;                                                                         \
		}                                                                                         \
		LIT_C++;                                                                                  \
		LIT_C++;                                                                                  \
		LIT_C++;                                                                                  \
		LIT_C++;                                                                                  \
	}
#define IS_HOUR_VALID(LIT_C)                                                                                                      \
	{                                                                                                                         \
		/* 00 - 23 */                                                                                                     \
		/* or */                                                                                                          \
		/* T00-T23*/                                                                                                      \
		const char *c = LIT_C;                                                                                            \
		if (('t' == *LIT_C) || ('T' == *LIT_C)) {                                                                         \
			/* ISO 8601 allows time to begin with a T */                                                              \
			LIT_C++;                                                                                                  \
			c++;                                                                                                      \
		}                                                                                                                 \
		if (!IS_END(c) && IS_NUMBER(c)) {                                                                                 \
			if (!IS_END(c) && IS_NUMBER(c + 1)) {                                                                     \
				/* 2 digit time */                                                                                \
				boolean_t first_digit_zero = FALSE, first_digit_one = FALSE, first_digit_two = FALSE;             \
				if ('0' == *c) {                                                                                  \
					first_digit_zero = TRUE;                                                                  \
				} else if ('1' == *c) {                                                                           \
					first_digit_one = TRUE;                                                                   \
				} else if ('2' == *c) {                                                                           \
					first_digit_two = TRUE;                                                                   \
				} else {                                                                                          \
					return 1;                                                                                 \
				}                                                                                                 \
				if (first_digit_zero || first_digit_one) {                                                        \
					/* 00-09 , 10-19 */                                                                       \
					/* The second digit is in valid range, it was confirmed by IS_NUMBER(c+1) call previously \
					 */                                                                                       \
				} else if (first_digit_two) {                                                                     \
					/* 20-23 */                                                                               \
					if (('0' <= *(c + 1)) && ('3' >= *(c + 1))) {                                             \
						/* Valid */                                                                       \
					} else {                                                                                  \
						return 1;                                                                         \
					}                                                                                         \
				} else {                                                                                          \
					assert(FALSE);                                                                            \
				}                                                                                                 \
				LIT_C++;                                                                                          \
				LIT_C++;                                                                                          \
			} else {                                                                                                  \
				/* 1 digit time */                                                                                \
				/* 0-9 is the valid range and the value is in it as confirmed by IS_NUMBER(c) call previously */  \
				LIT_C++;                                                                                          \
			}                                                                                                         \
		} else {                                                                                                          \
			return 1;                                                                                                 \
		}                                                                                                                 \
	}
#define IS_MIN_VALID(LIT_C)                                                                                                      \
	{                                                                                                                        \
		/* 00 - 59 */                                                                                                    \
		const char *c = LIT_C;                                                                                           \
		if (!IS_END(c) && IS_NUMBER(c)) {                                                                                \
			if (!IS_END(c + 1) && IS_NUMBER(c + 1)) {                                                                \
				/* 2 digit time */                                                                               \
				if ('5' < *c) {                                                                                  \
					return 1;                                                                                \
				}                                                                                                \
				LIT_C++;                                                                                         \
				LIT_C++;                                                                                         \
			} else {                                                                                                 \
				/* 1 digit time */                                                                               \
				/* 0-9 is the valid range and the value is in it as confirmed by IS_NUMBER(c) call previously */ \
				LIT_C++;                                                                                         \
			}                                                                                                        \
		} else {                                                                                                         \
			return 1;                                                                                                \
		}                                                                                                                \
	}

#define IS_MICROSECOND_VALID(LIT_C)                                                      \
	{                                                                                \
		const char *c = LIT_C;                                                   \
		if (!IS_END(c)) {                                                        \
			if ('.' == *c) {                                                 \
				LIT_C++;                                                 \
				const char *mc = LIT_C;                                  \
				if (IS_END(mc)) {                                        \
					/* micro second part is empty */                 \
					return 1;                                        \
				}                                                        \
				for (int i = 1; i <= 6; i++) {                           \
					if (IS_END(mc) || IS_GMT_OFFSET(mc)) {           \
						/* has less number of digits than 6 */   \
						break;                                   \
					}                                                \
					if (IS_NUMBER(mc)) {                             \
						mc++;                                    \
						LIT_C++;                                 \
					} else {                                         \
						/* non numeric */                        \
						return 1;                                \
					}                                                \
				}                                                        \
				/* Ignore higher precision values*/                      \
				while (IS_NUMBER(LIT_C)) {                               \
					LIT_C++;                                         \
				}                                                        \
			} else {                                                         \
				/* Something is not right or time zone is present.*/     \
				/* Let caller decide, exit as there is no microsecond */ \
			}                                                                \
		}                                                                        \
	}

#define IS_SEC_VALID(LIT_C)                                                                                                      \
	{                                                                                                                        \
		/* 00.000000 - 60.999999 */                                                                                      \
		const char *c = LIT_C;                                                                                           \
		if (IS_SEC_END(c)) {                                                                                             \
			if (IS_END(c)) {                                                                                         \
				return 1;                                                                                        \
			} else {                                                                                                 \
				/* .000000 to .999999 */                                                                         \
				IS_MICROSECOND_VALID(LIT_C);                                                                     \
			}                                                                                                        \
		}                                                                                                                \
		if (IS_NUMBER(c)) {                                                                                              \
			if (!IS_SEC_END(c + 1) && IS_NUMBER(c + 1)) {                                                            \
				/* 2 digit second value */                                                                       \
				if ('6' < *c) {                                                                                  \
					return 1;                                                                                \
				}                                                                                                \
				if (('6' == *c) && ('0' != *(c + 1))) {                                                          \
					return 1;                                                                                \
				}                                                                                                \
				LIT_C++;                                                                                         \
				LIT_C++;                                                                                         \
				IS_MICROSECOND_VALID(LIT_C);                                                                     \
			} else {                                                                                                 \
				/* 1 digit time */                                                                               \
				/* 0-9 is the valid range and the value is in it as confirmed by IS_NUMBER(c) call previously */ \
				LIT_C++;                                                                                         \
				IS_MICROSECOND_VALID(LIT_C);                                                                     \
			}                                                                                                        \
		} else {                                                                                                         \
			return 1;                                                                                                \
		}                                                                                                                \
	}

// Function to check if horolog value for date is in range
int is_date_in_horolog_range(char *literal) {
	// Expect `,` in the literal
	int	 length = 0;
	long int dateh;
	while ((',' != literal[length]) && ('\0' != literal[length])) {
		length++;
	}
	if (0 == length) {
		return 1;
	}
	if ('\0' == literal[length]) {
		// horolog input where only date value is seen in literal
		dateh = strtol(literal, NULL, 10);
	} else if (',' == literal[length]) {
		char  tmp_ch;
		char *dateh_str = literal;
		tmp_ch = dateh_str[length];
		dateh_str[length] = '\0';
		dateh = strtol(dateh_str, NULL, 10);
		dateh_str[length] = tmp_ch;
	} else {
		assert(FALSE);
		dateh = 0;
	}
	if ((-365 > dateh) || (2980013 < dateh)) {
		// Value not in range
		return 1;
	}
	return 0;
}

// Function to check if horolog value for time is in range
int is_time_in_horolog_range(char *literal) {
	// Expect `,` in the literal
	int	 length = 0;
	long int timeh;
	int	 micro = 0;
	while ((',' != literal[length]) && ('\0' != literal[length])) {
		length++;
	}
	if (',' == literal[length]) {
		// Time value is expected after the first `,`
		int sec_length = 0;
		length++;
		char *ch = &literal[length];
		while ((',' != literal[length]) && ('\0' != literal[length])) {
			length++;
			sec_length++;
		}
		if (0 == sec_length) {
			// Seconds information is not given, this is invalid
			return 1;
		}
		sec_length++; // Null terminator
		char  tmp_char;
		char *timeh_str = ch;
		tmp_char = timeh_str[sec_length - 1];
		timeh_str[sec_length - 1] = '\0';
		timeh = strtol(timeh_str, NULL, 10);
		timeh_str[sec_length - 1] = tmp_char;
		if (',' == literal[length]) {
			// Micro seconds exist (call from zhorolog)
			int micro_sec_length = 0;
			length++;
			char *ch = &literal[length];
			while ((',' != literal[length]) && ('\0' != literal[length])) {
				length++;
				micro_sec_length++;
			}
			if (1 <= micro_sec_length) {
				micro_sec_length++; // Null terminator
				char *micro_str = ch;
				tmp_char = micro_str[micro_sec_length - 1];
				micro_str[micro_sec_length - 1] = '\0';
				ATOI(micro_str, micro);
				micro_str[micro_sec_length - 1] = tmp_char;
			} // else, no micro-second information given this is okay.
		}
	} else if ('\0' == literal[length]) {
		// This is only possible for horolog format where there is only one value in the literal
		timeh = strtol(literal, NULL, 10);
	} else {
		assert(FALSE);
		timeh = 0;
	}
	if ((0 > timeh) || (86399 < timeh)) {
		return 1;
	}
	if ((0 > micro) || (999999 < micro)) {
		return 1;
	}
	return 0;
}

// Function to check if zhorolog value for timezone is in range
int is_time_zone_in_zhorolog_range(char *literal) {
	// ,,,43200 to ,,,-50400
	char *time_zone_loc = strrchr(literal, ',');
	time_zone_loc++; // Move past ','
	int time_zone;
	ATOI(time_zone_loc, time_zone);
	if ((-50400 > time_zone) || (43200 < time_zone)) {
		return 1;
	}
	return 0;
}

int is_date_in_fileman_range(char *literal) {
	// Expect `.` in the literal
	int  length = 0;
	char year[4], month[3], day[3];
	if (('-' == literal[length]) || ('+' == literal[length])) {
		// Do not expect any symbols
		return 1;
	}
	while (('.' != literal[length]) && ('\0' != literal[length])) {
		length++;
	}
	if (7 != length) {
		// Expected to be only 7 digits and a period or null
		// YYYMMDD. OR YYYMMDD
		if ((3 == length) && ('\0' == literal[length])) {
			// This is YYY
			return 1;
		} else {
			return 1;
		}
	}
	if ('.' == literal[length]) {
		memcpy(year, literal, 3);
		year[3] = '\0';
		memcpy(month, literal + 3, 2);
		month[2] = '\0';
		memcpy(day, literal + 5, 2);
		day[2] = '\0';
	} else {
		assert('\0' == literal[length]);
		memcpy(year, literal, 3);
		year[3] = '\0';
		memcpy(month, literal + 3, 2);
		month[2] = '\0';
		memcpy(day, literal + 5, 2);
		day[2] = '\0';
	}
	int year_int;
	ATOI(year, year_int);
	UNUSED(year_int); // Avoid [-Wunused-variable]
	int mon_int;
	ATOI(month, mon_int);
	int day_int;
	ATOI(day, day_int);
	assert((0 <= year_int) && (999 >= year_int));
	assert((0 <= mon_int) && (0 <= day_int));
	if ((12 < mon_int) || (31 < day_int)) {
		return 1;
	}
	// Validate in-exact dates
	if (0 == year_int) {
		// error
		return 1;
	} else if ((0 == mon_int) && (0 != day_int)) {
		// error
		return 1;
	}
	return 0;
}

int is_time_in_fileman_range(char *literal) {
	// Expect `.` in the literal
	int length = 0, time_len;
	while (('.' != literal[length]) && ('\0' != literal[length])) {
		length++;
	}
	if ('.' == literal[length]) {
		time_len = strlen(literal + length) - 1; // remove '.' length
		length = length + 1;			 // point to time part
	} else {
		return 0; // Allow values where time is not given
	}
	if (6 < time_len) {
		// time part too long
		// HHMMSS
		return 1;
	}
	char hour[3], min[3], sec[3];
	hour[0] = sec[0] = min[0] = '0';
	hour[1] = sec[1] = min[1] = '0';
	hour[2] = sec[2] = min[2] = '\0';
	if (2 <= time_len) {
		memcpy(hour, &literal[length], 2);
	}
	if (3 <= time_len) {
		memcpy(min, &literal[length + 2], 2);
	}
	if (5 <= time_len) {
		memcpy(sec, &literal[length + 4], 2);
	}

	int hour_int;
	ATOI(hour, hour_int);
	if ((2 == time_len) && ((10 == hour_int) || (20 == hour_int))) {
		// 10 or 20 for hour is invalid when minute and second is absent
		return 1;
	} else if ((1 == time_len) && (3 <= hour_int)) {
		// A single digit hour should not be greater than 2 (2960124.3)
		return 1;
	}

	int min_int;
	ATOI(min, min_int);
	if ((3 == time_len) && (6 <= min_int)) {
		// A single digit minute should not be greater than 5 (2960124.166)
		return 1;
	}

	int sec_int;
	ATOI(sec, sec_int);
	if ((5 == time_len) && (6 <= sec_int)) {
		// A single digit second should not be greater than 5 (2960124.16266)
		return 1;
	}

	if (24 < hour_int) {
		return 1;
	}
	if (24 == hour_int) {
		if ((0 != min_int) || (0 != sec_int)) {
			return 1;
		} // else 240000
	} else {
		if (59 < min_int) {
			return 1;
		}
		if (59 < sec_int) {
			return 1;
		}
	}
	return 0;
}

// Function to validate time and date based on format given
int is_date_time_literal_in_valid_format(SqlValueType date_time_type, char *literal, char *text_format) {
	/* Get internal format
	 * Date
	 *	%m-%d-%Y
	 *	%Y-%m-%d
	 *	%d-%m-%Y
	 * Time
	 *	%H:%M:%S
	 * Timestamp
	 *	%m-%d-%Y %H:%M:%S
	 */
	int	    ret = 0;
	const char *format = text_format;
	boolean_t   is_time_zone_allowed = FALSE;
	if (DATE_LITERAL == date_time_type) {
	} else if (TIME_LITERAL == date_time_type) {
		is_time_zone_allowed = TRUE; // Allowed just for flexibility, value is ignored
	} else if (TIMESTAMP_LITERAL == date_time_type) {
		is_time_zone_allowed = TRUE; // Allowed just for flexibility, value is ignored
	} else if (TIME_WITH_TIME_ZONE_LITERAL == date_time_type) {
		is_time_zone_allowed = TRUE;
	} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type) {
		is_time_zone_allowed = TRUE;
	} else {
		assert(FALSE);
		format = ""; /* Prevents [-Wmaybe-uninitialized] */
	}
	const char *fmt_c = format;
	char	   *lit_c = literal;
	boolean_t   is_month_parsed = FALSE;
	boolean_t   is_day_parsed = FALSE;
	boolean_t   is_year_parsed = FALSE;
	/* Skip arbitrary spaces before the actual value. strptime allows this but is not documented so it is removed later on in
	 * ydboctoText2InternalFormatC.
	 */
	while (' ' == *lit_c) {
		lit_c++;
	}
	while ('\0' != *fmt_c) {
		switch (*fmt_c) {
		case '%':
			// Move on to the next character
			break;
		case 'm':
			IS_MONTH_VALID(lit_c);
			is_month_parsed = TRUE;
			break;
		case 'd':
			IS_DATE_VALID(lit_c);
			is_day_parsed = TRUE;
			break;
		case 'Y':
			IS_YEAR_VALID(lit_c);
			is_year_parsed = TRUE;
			break;
		case 'H':
			if (IS_END(lit_c)) {
				if ((TIMESTAMP_LITERAL == date_time_type) || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type)) {
					// This is a timestamp without time information. Consider this as valid.
					return ret;
				}
			}
			IS_HOUR_VALID(lit_c);
			break;
		case 'M':
			IS_MIN_VALID(lit_c);
			break;
		case 'S':
			IS_SEC_VALID(lit_c);
			break;
		case 'z':
			IS_TIME_ZONE(lit_c);
			break;
		case '-':
		case ':':
		case ' ':
			if ((' ' == *fmt_c)
			    && !((TIMESTAMP_LITERAL == date_time_type) || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type))) {
				return 1;
			}
			if (*lit_c != *fmt_c) {
				if ((' ' == *fmt_c) && (('T' == *lit_c) || ('t' == *lit_c))) {
					if ((TIMESTAMP_LITERAL == date_time_type)
					    || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type)) {
						// This is valid, ISO 8601 allows time to begin with a T
						lit_c++;
						fmt_c++;
						// Check if there are more T's this is allowed in Postgres so we allow that here too
						while (('T' == *lit_c) || ('t' == *lit_c)) {
							lit_c++;
						}
						continue;
					}
				} else if ((' ' == *fmt_c) && IS_END(lit_c)) {
					if ((TIMESTAMP_LITERAL == date_time_type)
					    || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type)) {
						// This is a timestamp without time information. Consider this as valid.
						return ret;
					}
				}
				// Literal doesn't match expected format
				return 1;
			} else {
				if (' ' == *lit_c) {
					/* Allow arbitrary number of spaces between date and time. `strptime` accepts this. */
					assert((' ' == *fmt_c)
					       && ((TIMESTAMP_LITERAL == date_time_type)
						   || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type)));
					while (' ' == *lit_c) {
						lit_c++;
					}
				} else {
					lit_c++;
				}
			}
			break;
		default:
			// Un-expected character
			return 1;
			break;
		}
		fmt_c++;
	}
	if (!IS_END(lit_c)) {
		// The input has more characters than what the format allows. This is in-valid
		if (is_time_zone_allowed) {
			if (' ' == *lit_c) {
				while (' ' == *lit_c) {
					// Skip white spaces
					lit_c++;
				}
				if (!IS_END(lit_c)) {
					return 1;
				}
			}
			IS_TIME_ZONE(lit_c);
		} else {
			if (((' ' == *lit_c) || ('T' == *lit_c) || ('t' == *lit_c)) && (DATE_LITERAL == date_time_type)
			    && is_year_parsed && is_month_parsed && is_day_parsed) {
				// '01-01-2023 01:01:01 is a valid date
				// Do ensure remaining string is also valid
				lit_c++;
				while (' ' == *lit_c) {
					// Skip white spaces
					lit_c++;
				}
				if (!IS_END(lit_c)) {
					IS_HOUR_VALID(lit_c);
					if (':' == *lit_c) {
						lit_c++;
					} else {
						return 1;
					}
					IS_MIN_VALID(lit_c);
					if (':' == *lit_c) {
						lit_c++;
					} else {
						return 1;
					}
					IS_SEC_VALID(lit_c);
					if (' ' == *lit_c) {
						while (' ' == *lit_c) {
							// Skip white spaces
							lit_c++;
						}
						if (!IS_END(lit_c)) {
							return 1;
						}
					}
					IS_TIME_ZONE(lit_c);
				}
			} else {
				if (' ' == *lit_c) {
					while (' ' == *lit_c) {
						// Skip white spaces
						lit_c++;
					}
					if (!IS_END(lit_c)) {
						return 1;
					}
				}
				IS_TIME_ZONE(lit_c);
			}
		}
	}
	return ret;
}

int is_all_numbers_and_has_specified_num_of_delims(char *str, char delim, int num_of_delims) {
	int   delim_count = 0, num_count = 0;
	char *c = str;
	if (('+' == *c) || ('-' == *c)) {
		// valid, move on to next char
		c++;
	}
	while ('\0' != *c) {
		if (delim == *c) {
			delim_count++;
			if (delim_count > num_of_delims) {
				return 1;
			} else if (('\0' != *(c + 1)) && (('+' == *(c + 1)) || ('-' == *(c + 1)))) {
				// delim is followed by a + or -
				/* Move one more character so that the later increment can skip both chars
				 * as both are valid.
				 */
				c++;
				if (('\0' == *(c + 1)) || !(('0' <= *(c + 1)) && ('9' >= *(c + 1)))) {
					// The + or - is not followed by a decimal digit
					return 1;
				}
			}
		} else if (('0' <= *c) && ('9' >= *c)) {
			// valid
			num_count++;
		} else {
			return 1;
		}
		c++;
	}
	if (0 == num_count) {
		return 1;
	}
	if (delim_count < num_of_delims) {
		return 1;
	}
	return 0;
}

int is_date_time_value_in_zut_range(char *value, SqlValueType type) {
	// Date: -62135596800000000 (01-01-0001) to 253402214400000000 (12-31-9999)
	// Timestamp: -62135596800000000 (01-01-0000 00:00:00.000000) to 253402300799999999(12-31-9999 23:59:59.999999)
	if ((TIME_LITERAL == type) || (TIME_WITH_TIME_ZONE_LITERAL == type)) {
		return 1;
	}
	long int int_value = strtol(value, NULL, 10);
	switch (type) {
	case DATE_LITERAL:
		if ((-62135596800000000 <= int_value) && (253402214400000000 >= int_value)) {
			return 0;
		}
		break;
	case TIMESTAMP_LITERAL:
		if ((-62135596800000000 <= int_value) && (253402300799999999 >= int_value)) {
			return 0;
		}
		break;
	default:
		assert(FALSE);
		break;
	}
	return 1;
}

/* This function validates the given literal to be a valid date_time_type and internal_format value.
 */
int validate_date_time_value(char **literal_ptr, SqlValueType date_time_type, OptionalKeyword internal_format, char *text_format) {
	int   ret = 0;
	char *literal = *literal_ptr;
	switch (internal_format) {
	case OPTIONAL_DATE_TIME_HOROLOG:
		// Example: 66753,49110
		if (DATE_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 0);
			if (0 != ret) {
				ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 1);
				if (0 != ret) {
					break;
				}
			}
			ret = is_date_in_horolog_range(literal);
		} else if (TIME_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 0);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		} else if (TIMESTAMP_LITERAL == date_time_type) {
			// Check that only decimal digits and 1 `,` exists
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 1);
			if (0 != ret) {
				break;
			}
			ret = is_date_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		} else if (TIME_WITH_TIME_ZONE_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 0);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type) {
			// Check that only decimal digits and 1 `,` exists
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 1);
			if (0 != ret) {
				break;
			}
			ret = is_date_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		}
		break;
	case OPTIONAL_DATE_TIME_ZHOROLOG:
		if (DATE_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 3);
			if (0 != ret) {
				break;
			}
			ret = is_date_in_horolog_range(literal);
		} else if (TIME_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 3);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		} else if (TIMESTAMP_LITERAL == date_time_type) {
			// Check that only decimal digits and 1 `,` exists
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 3);
			if (0 != ret) {
				break;
			}
			ret = is_date_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
		} else if (TIME_WITH_TIME_ZONE_LITERAL == date_time_type) {
			// Check that only decimal digits and max 4 `,` exists
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 3);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_zone_in_zhorolog_range(literal);
		} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type) {
			// Check that only decimal digits and max 4 `,` exists
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, ',', 3);
			if (0 != ret) {
				break;
			}
			ret = is_date_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_in_horolog_range(literal);
			if (0 != ret) {
				break;
			}
			ret = is_time_zone_in_zhorolog_range(literal);
		}
		break;
	case OPTIONAL_DATE_TIME_FILEMAN:
		if (DATE_LITERAL == date_time_type) {
			// Check that only decimal digits exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 0);
			if (0 != ret) {
				ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 1); // no time info
				if (0 != ret) {
					assert(1 == ret);
					return ret;
				}
			}
			ret = is_date_in_fileman_range(literal);
		} else if (TIME_LITERAL == date_time_type) {
			return -1; // This is strictly not allowed
		} else if (TIMESTAMP_LITERAL == date_time_type) {
			// Check that only decimal digits and a period exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 1);
			if (0 != ret) {
				ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 0); // no time info
				if (0 != ret) {
					assert(1 == ret);
					return ret;
				}
			}
			ret = is_date_in_fileman_range(literal);
			if (0 != ret) {
				assert(1 == ret);
				return ret;
			}
			ret = is_time_in_fileman_range(literal);
		} else if (TIME_WITH_TIME_ZONE_LITERAL == date_time_type) {
			ret = -1;
		} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type) {
			// Check that only decimal digits and a period exist
			ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 1);
			if (0 != ret) {
				ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 0); // no time info
				if (0 != ret) {
					assert(1 == ret);
					return ret;
				}
			}
			ret = is_date_in_fileman_range(literal);
			if (0 != ret) {
				assert(1 == ret);
				return ret;
			}
			ret = is_time_in_fileman_range(literal);
		}
		break;
	case OPTIONAL_DATE_TIME_ZUT:
		// - or + should not be allowed here
		// Check that only decimal digits exist
		ret = is_all_numbers_and_has_specified_num_of_delims(literal, '.', 0);
		if (0 != ret) {
			assert(1 == ret);
			return ret;
		}
		ret = is_date_time_value_in_zut_range(literal, date_time_type);
		break;
	case OPTIONAL_DATE_TIME_TEXT:;
		ret = is_date_time_literal_in_valid_format(date_time_type, literal, text_format);
		break;
	default:
		assert(FALSE);
		break;
	}
	assert((0 == ret) || (1 == ret));
	return ret;
}

/* Used to compute the 9's compliment of a given value
 * 	`VAL` is microsecond value as an integer, valid only when `ARR` is NULL
 * 	`ARR` is pointer to the microseconds array
 * Mainly called to form the internal format value of a date/time which is
 * before EPOCH. Before EPOCH the seconds part of the internal format will be negative
 * so we need to convert the microseconds value to be a negative number also. This macro is
 * used for the conversion.
 * Similar logic is used in PrintDateTimeResultColumnValue() of src/aux/_ydboctoplanhelpers.m
 * to process microsecond when output format is ZUT (Search for NINES_COMPLIMENT). Any change
 * here should reflect there.
 */
#define NINES_COMPLIMENT(VAL, ARR)                                          \
	{                                                                   \
		if (NULL != ARR) {                                          \
			char *val = ARR;                                    \
			for (int i = 5; i > -1; i--) {                      \
				assert(('0' <= val[i]) && ('9' >= val[i])); \
				val[i] = '9' - val[i] + '0';                \
			}                                                   \
		} else {                                                    \
			assert(0 <= (VAL));                                 \
			(VAL) = 999999 - (VAL);                             \
		}                                                           \
	}

/* This macro appends microseconds part given by `micro` to `value`.
 * If `y` is `micro` and `x` is `value the result will be `xy`.
 */
#define ADD_MICRO_SECONDS(VALUE, MICRO)                       \
	{                                                     \
		int lcl_micro = MICRO;                        \
		if (0 > VALUE) {                              \
			assert(0 <= MICRO);                   \
			lcl_micro = MICRO;                    \
			NINES_COMPLIMENT(lcl_micro, NULL);    \
		}                                             \
		int micro_arr[6] = {0, 0, 0, 0, 0, 0};        \
		int i = 5;                                    \
		while (0 != lcl_micro) {                      \
			micro_arr[i--] = (lcl_micro % 10);    \
			lcl_micro /= 10;                      \
		}                                             \
		boolean_t is_negative = (0 > VALUE);          \
		for (i = 0; i < 6; i++) {                     \
			VALUE = VALUE * 10;                   \
			if (is_negative) {                    \
				VALUE = VALUE - micro_arr[i]; \
			} else {                              \
				VALUE = VALUE + micro_arr[i]; \
			}                                     \
		}                                             \
	}

/* This macro removes microseconds part in `value` and returns it using `ret`.
 * `value` is date/time data in internal format.
 */
#define REMOVE_MICRO_SECONDS(VALUE, RET)                    \
	{                                                   \
		int lcl_micro = 0;                          \
		int mult = 1;                               \
		for (int i = 0; i < 6; i++) {               \
			lcl_micro += ((VALUE % 10) * mult); \
			VALUE /= 10;                        \
			mult *= 10;                         \
		}                                           \
		if (0 > lcl_micro) {                        \
			/* Ignore -ve sign */               \
			lcl_micro = -lcl_micro;             \
		}                                           \
		if (0 > VALUE) {                            \
			NINES_COMPLIMENT(lcl_micro, NULL);  \
		}                                           \
		RET = lcl_micro;                            \
	}

/* This function converts a given date/time value to the date/time value expected by ydboctoText2InternalFormatC().
 *
 * Changes done:
 *   1) `T` is replaced with a space. If `T` appears as the first character (can happen if value belongs to TIME type) it is
 * ignored. 2) Sub-seconds greater than 6 precision is removed 3) Return value is NULL terminated as its needed by
 * ydboctoText2InternalFormatC()
 *
 * For ex:
 *   1) 2024-02-21T13:31:48.05098021+07:00 -> 2024-02-21 13:31:48.050980+07:00
 *   2) `T01:01:01` -> `01:01:01`
 *
 * Input:
 * `length` is the size of `time_str`
 *
 * Output:
 * `new_str` is return value
 */
void convert_to_std_time_lit(char *time_str, int length, char *new_str) {
	assert(NULL != new_str);
	int	  iter = 0, new_iter = 0;
	int	  sub_second_count = 1;
	boolean_t is_sub_second = FALSE;
	while (iter < length) {
		if (('T' == time_str[iter]) || ('t' == time_str[iter])) {
			// Handle T separator
			if (0 == iter) {
				// The beginning character is a T, this can happen if the value is of time type, ignore it
			} else {
				/* Arbitrary number of T's can exist between date and time and this is replaced by spaces here.
				 * Space in the format string passed to strptime() matches 0 or more spaces so we expect
				 * the spaces added here to be processed correctly by later code that makes use of strptime().
				 */
				new_str[new_iter++] = ' ';
			}
		} else if ('.' == time_str[iter]) {
			// Begin sub-second handling
			is_sub_second = TRUE;
			new_str[new_iter++] = time_str[iter];
		} else if (is_sub_second) {
			if (('+' == time_str[iter]) || ('-' == time_str[iter])) {
				// End sub-second handling
				is_sub_second = FALSE;
				new_str[new_iter++] = time_str[iter];
			} else if (sub_second_count <= 6) {
				// Add to return array as precision is still less than 6
				new_str[new_iter++] = time_str[iter];
				sub_second_count++;
			} else {
				// This is a sub-second but we ignore these as its greater than 6 precision
			}
		} else {
			// Add to return array as this is not a T or a sub-second
			new_str[new_iter++] = time_str[iter];
		}
		iter++;
	}
	// Null terminate the result
	new_str[new_iter] = '\0';
}

ydb_long_t utc_mktime(struct tm *tm1) {
	// Change TZ to UTC
	ydb_long_t ret;
	char	  *orig_tz_val_env, *orig_tz_val;
	int	   ret_setenv;
	boolean_t  tz_unset = FALSE;

	orig_tz_val_env = getenv("TZ");
	if (NULL == orig_tz_val_env) {
		orig_tz_val_env = "";
		tz_unset = TRUE;
	}
	orig_tz_val = strdup(orig_tz_val_env);

	ret_setenv = setenv("TZ", "UTC", 1);
	assert(0 == ret_setenv);
	UNUSED(ret_setenv); // Prevents clang-analyzer-deadcode.DeadStores
	/* A positive value means DST is in effect; zero means that DST is not in effect;
	 * and a negative value means that mktime() should (use timezone in-formation and system databases to)
	 * attempt to determine whether DST is in effect at the specified time.
	 */
	tm1->tm_isdst = -1;
	ret = mktime(tm1);
	// Change TZ back to original value
	ret_setenv = setenv("TZ", orig_tz_val, 1);
	assert(0 == ret_setenv);
	UNUSED(ret_setenv); // Prevents clang-analyzer-deadcode.DeadStores
	if (tz_unset) {
		unsetenv("TZ"); // This is needed to avoid next call to use UTC value from the variables which tzset sets
	}
	free(orig_tz_val);
	return ret;
}

/* This function converts the input `secs` which is in unix time and `microsec` sub second value to internal format value */
ydb_long_t ydboctoZutC(int count, ydb_long_t secs, ydb_int_t microsec, ydb_string_t *format) {
	time_t sec = secs;
	// Get the format
	char *value_str_format;
	value_str_format = ydb_malloc(format->length + 1);
	memcpy(value_str_format, format->address, format->length);
	value_str_format[format->length] = '\0';
	boolean_t  include_time = (NULL != strchr(value_str_format, 'H'));
	ydb_long_t ret;
	if (include_time) {
		// sec is already in unix time, just combine it with microseconds and return
		ret = sec;
	} else {
		// gmtime as Zut is in utc
		struct tm *static_tm = gmtime(&sec);
		struct tm  tm1;
		memcpy(&tm1, static_tm, sizeof(struct tm));
		tm1.tm_min = 0;
		tm1.tm_hour = 0;
		tm1.tm_sec = 0;
		microsec = 0;
		ret = utc_mktime(&tm1);
	}
	ADD_MICRO_SECONDS(ret, microsec);
	ydb_free(value_str_format);
	return ret;
}

time_t convertToLocalTimezone(SqlValueType type, time_t val) {
	time_t	   op1_time = val;
	struct tm *tm1;
	int	   gmtoff;
	if (TIME_WITH_TIME_ZONE_LITERAL == type) {
		struct tm *tm2;
		// Get current date
		time_t current_time;
		current_time = time(NULL);
		// Get struct tm
		tm2 = localtime(&current_time);
		gmtoff = tm2->__tm_gmtoff;
	} else {
		tm1 = localtime(&op1_time);
		gmtoff = (*tm1).__tm_gmtoff;
	}
	return op1_time + gmtoff;
}

/* This function converts the internal format data in `value` to type given by `output_type` from `input_type`.
 * `input_type` is SqlDataType and is the type in which `value` is in
 * `output_type` is SqlValueType and is the type in which `value` needs to be casted to
 * The returned value is in internal format.
 */
ydb_long_t ydboctoDateTimeCastC(int count, ydb_long_t value, ydb_int_t date_time_input_type, ydb_int_t date_time_output_type) {
	UNUSED(count);
	// Convert value to tm structure
	struct tm  tm1;
	struct tm *static_tm;
	// Before calling time function remove the micro second portion in value
	// micro = last 6 digits
	long int micro;
	REMOVE_MICRO_SECONDS(value, micro);
	// Get the value type
	SqlValueType input_type = date_time_input_type;
	SqlDataType  output_type = date_time_output_type;
	if ((DATE_LITERAL == input_type) || (TIME_LITERAL == input_type) || (TIMESTAMP_LITERAL == input_type)) {
		static_tm = gmtime(&value);
	} else {
		static_tm = localtime(&value);
	}
	memcpy(&tm1, static_tm, sizeof(struct tm));
	ydb_long_t ret;
	if (TIME_TYPE == output_type) {
		// Keep only date related info in tm1 and form the return value
		if (TIME_LITERAL == input_type) {
			// Nothing to do here as its already in date format
			ret = value;
		} else if (TIME_WITH_TIME_ZONE_LITERAL == input_type) {
			ret = utc_mktime(&tm1);
		} else if (TIMESTAMP_LITERAL == input_type) {
			tm1.tm_mday = 0;
			tm1.tm_mon = 0;
			tm1.tm_year = 0;
			ret = utc_mktime(&tm1);
		} else {
			assert(TIMESTAMP_WITH_TIME_ZONE_LITERAL == input_type);
			tm1.tm_mday = 0;
			tm1.tm_mon = 0;
			tm1.tm_year = 0;
			ret = utc_mktime(&tm1);
		}
	} else if (DATE_TYPE == output_type) {
		if (DATE_LITERAL == input_type) {
			// Nothing to do here as its already in date format
			ret = value;
		} else if (TIMESTAMP_LITERAL == input_type) {
			tm1.tm_sec = 0;
			tm1.tm_min = 0;
			tm1.tm_hour = 0;
			ret = utc_mktime(&tm1);
		} else {
			assert(TIMESTAMP_WITH_TIME_ZONE_LITERAL == input_type);
			tm1.tm_sec = 0;
			tm1.tm_min = 0;
			tm1.tm_hour = 0;
			ret = mktime(&tm1);
		}
		micro = 0; // Remove any microsecond value
	} else if (TIMESTAMP_TYPE == output_type) {
		if (DATE_LITERAL == input_type) {
			tm1.tm_sec = 0;
			tm1.tm_min = 0;
			tm1.tm_hour = 0;
			ret = utc_mktime(&tm1);
		} else if (TIMESTAMP_LITERAL == input_type) {
			// Nothing to do here as its already in timestamp format
			ret = value;
		} else {
			assert(TIMESTAMP_WITH_TIME_ZONE_LITERAL == input_type);
			// Value is already in current time zone, save it as it is in UTC
			ret = utc_mktime(&tm1);
		}
	} else if (TIME_WITH_TIME_ZONE_TYPE == output_type) {
		if (TIME_LITERAL == input_type) {
			// Nothing to do here as its already in time format
			ret = mktime(&tm1);
		} else {
			assert(TIME_WITH_TIME_ZONE_LITERAL == input_type);
			// Nothing to do here as its already in time format
			ret = value;
		}
	} else {
		assert(TIMESTAMP_WITH_TIME_ZONE_TYPE == output_type);
		if (DATE_LITERAL == input_type) {
			tm1.tm_sec = 0;
			tm1.tm_min = 0;
			tm1.tm_hour = 0;
			tm1.tm_isdst = -1;
			ret = mktime(&tm1);
		} else if (TIMESTAMP_LITERAL == input_type) {
			tm1.tm_isdst = -1;
			ret = mktime(&tm1);
		} else {
			assert(TIMESTAMP_WITH_TIME_ZONE_LITERAL == input_type);
			ret = value;
		}
	}
	ADD_MICRO_SECONDS(ret, micro);
	return ret;
}

/*
 * This function converts the given date/time value in internal format to Text format. Config is used
 * to determine the date/time result format.
 * Input:
 * 	date_time_type: A value of SqlValueType enum used to determine to which date/time type the value belongs to
 *	value: A date/time value in internal format that needs to be converted
 * Result:
 * 	Date/time value as text in the format given in config
 */
ydb_string_t *ydboctoDateTimeInternalFormat2TextC(int count, ydb_long_t value, ydb_int_t date_time_type,
						  ydb_string_t *text_format_specifier) {
	SqlValueType type = date_time_type;
	char	    *result;
	time_t	     date_time_value = value;

	// Get format
	char *format;
	format = ydb_malloc(text_format_specifier->length + 1);
	memcpy(format, text_format_specifier->address, text_format_specifier->length);
	format[text_format_specifier->length] = '\0';

	/* Internal format will have the last 6 digits as microseconds.
	 * Remove that and then pass the time functions.
	 */
	long int micro = 0;
	REMOVE_MICRO_SECONDS(date_time_value, micro);

	// Convert internal format unix time to output format
	struct tm *tm1;
	if ((DATE_LITERAL == date_time_type) || (TIMESTAMP_LITERAL == date_time_type) || (TIME_LITERAL == date_time_type)) {
		tm1 = gmtime(&date_time_value);
	} else {
		// date_time_value = convertToLocalTimezone(date_time_value);
		if (TIME_WITH_TIME_ZONE_LITERAL == date_time_type) {
			/* Following code is to bring the time with time zone value to present time zone. This
			 * helps to consider daylight savings. Date at the time of execution is considered to be the date
			 * in which the time is specified to be.
			 */
			// Get current date
			time_t current_time;
			current_time = time(NULL);
			// Get struct tm
			struct tm *tm2;
			tm2 = localtime(&current_time);
			// Get day, month, year
			int day = tm2->tm_mday;
			int month = tm2->tm_mon;
			int year = tm2->tm_year;
			// Get local time of the time with time zone value without date info
			tm1 = localtime(&date_time_value);
			// Add date info to it
			tm1->tm_mday = day;
			tm1->tm_mon = month;
			tm1->tm_year = year;
			/* Following mktime will consider the date value and modify struct tm accordingly
			 * i.e. time zone with daylight savings will be considered as date is part of the info given to it.
			 */
			mktime(tm1);
		} else {
			tm1 = localtime(&date_time_value);
		}
	}

	// Convert result to ydb_string_t type
	ydb_string_t *ret;
	// This allocation will be freed not in Octo but by YottaDB after the external call returns.
	ret = ydb_malloc(sizeof(ydb_string_t));
	// This allocation will be freed not in Octo but by YottaDB after the external call returns.
	ret->address = ydb_malloc(sizeof(char) * 40); // Null terminator included
	char ret_str[40];
	int  length = 0;
	memset(ret_str, '\0', 40);
	length = strftime(ret_str, 40, format, tm1);

	boolean_t no_time_zone = TRUE;
	if ((TIME_WITH_TIME_ZONE_LITERAL == date_time_type) || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == date_time_type)) {
		if (NULL != strchr(format, 'z')) {
			/* In mysql emulation we will not have %z as format specifier.
			 * This is intensionally done to avoid timezone in return value.
			 * Also, in this case type will not be modified by the caller to TIME_LITERAL and TIMESTAMP_LITERAL as
			 * original type information is needed here to determine which function is selected to convert unix time to
			 * struct tm value.
			 */
			no_time_zone = FALSE;
		}
	}

	if (DATE_LITERAL == date_time_type) {
		ret->length = sprintf(ret->address, "%s", ret_str);
	} else {
		if (no_time_zone) {
			ret->length = sprintf(ret->address, "%s", ret_str);
			if (0 != micro) {
				ret->length += sprintf((ret->address + ret->length), ".%06ld", micro);
				// No easy way to ensure trailing 0's are trimmed so do it manually
				while ('0' == ret->address[ret->length - 1]) {
					ret->length--;
				}
			}
		} else {
			// Time zone is present in the result, add micro seconds to the result carefully as it needs to come before
			// timezone
			// Time zone is always 5 in length
			char time_zone[8];
			strncpy(time_zone, ret_str + length - 5, 5);
			// copy timezone seconds as well
			int timezone_sec = (tm1->__tm_gmtoff % 3600) % 60;
			if (0 > timezone_sec) {
				timezone_sec = -timezone_sec;
			}
			time_zone[5] = '0' + (timezone_sec / 10);
			time_zone[6] = '0' + (timezone_sec % 10);
			time_zone[7] = '\0';
			ret_str[length - 5] = '\0';
			ret->length = sprintf(ret->address, "%s", ret_str);
			if (0 != micro) {
				ret->length += sprintf((ret->address + ret->length), ".%06ld", micro);
				// No easy way to ensure trailing 0's are trimmed so do it manually
				while ('0' == ret->address[ret->length - 1]) {
					ret->length--;
				}
			}
			// Add time zone now
			// Add time zone hour with + or - sign first
			strncpy(ret->address + ret->length, time_zone, 3);
			ret->length += 3;
			if (('0' == time_zone[3]) && ('0' == time_zone[4]) && ('0' == time_zone[5]) && ('0' == time_zone[6])) {
				// Don't add this part
			} else {
				ret->address[ret->length++] = ':';
				strncpy(ret->address + ret->length, time_zone + 3, 2);
				ret->length += 2;
				if (('0' != time_zone[5]) || ('0' != time_zone[6])) {
					ret->address[ret->length++] = ':';
					strncpy(ret->address + ret->length, time_zone + 5, 2);
					ret->length += 2;
				}
			}
		}
	}
	ret->address[ret->length] = '\0';
	ydb_free(format);
	return ret;
}

ydb_long_t ydboctoConvertToLocalTimezoneC(int count, ydb_int_t type, ydb_long_t date_time_utc_value) {
	// Be mindful of the microseconds
	long int micro_op1;
	REMOVE_MICRO_SECONDS(date_time_utc_value, micro_op1);
	SqlValueType op_type = type;
	ydb_long_t   ret = convertToLocalTimezone(type, date_time_utc_value);
	ADD_MICRO_SECONDS(ret, micro_op1);
	return ret;
}

ydb_string_t *ydboctoText2InternalFormatC(int count, ydb_string_t *op1, ydb_string_t *format) {
	UNUSED(count);

	// Get format
	char *time_format;
	time_format = ydb_malloc(format->length + 1);
	memcpy(time_format, format->address, format->length);
	time_format[format->length] = '\0';

	// Convert op1 to tm structure
	struct tm tm1;
	// 0-initialize fields of tm struct
	memset(&tm1, 0, sizeof(struct tm));
	/* A TEXT format input can have literals of different kinds (2024-02-21T13:31:48.05098021+07:00,
	 * 2024-02-21 13:31:48.05098021+07:00). Convert them to Y-M-D H:M:S.UUUUUU+/-timezone and NULL terminate the string
	 * as it is needed by strptime and a ydb_string_t is not guaranteed to be null terminated.
	 */
	char *time_str = ydb_malloc(op1->length + 1);
	convert_to_std_time_lit(op1->address, op1->length, time_str);

	boolean_t include_time_zone = FALSE;
	// If time is not included then micro_second and timezone will not be used so don't bother processing them
	boolean_t include_time = (NULL != strchr(time_format, 'S'));
	assert(include_time
	       || (!include_time)
		      && (NULL == strchr(time_format, 'H') && (NULL == strchr(time_format, 'M'))
			  && (NULL == strchr(time_format, 'z'))));
	char	 *micro_second_str_ptr;
	char	  micro[7] = {'0', '0', '0', '0', '0', '0', '\0'};
	boolean_t time_zone_set = FALSE;
	if (include_time) {
		char *orig_micro_second_str_ptr;
		orig_micro_second_str_ptr = micro_second_str_ptr = strchr(time_str, '.');
		include_time_zone = (NULL != strchr(time_format, 'z'));
		// Remove microseconds as it is not recognized by srptime
		if (NULL == micro_second_str_ptr) {
			// Micro seconds not present
		} else {
			// Get microseconds
			micro_second_str_ptr++; // Moving one location next to '.'
			char *plus_or_minus = micro_second_str_ptr;
			int   length = 0;
			// Traverse the string to get only the microsecond part
			while ('\0' != *plus_or_minus) {
				if (('+' == *plus_or_minus) || ('-' == *plus_or_minus)) {
					// Found timezone part
					break;
				}
				micro[length] = *plus_or_minus;
				length++;
				plus_or_minus++;
			}
			// Pad any remaining locations with 0's
			while (length < 6) {
				micro[length++] = '0';
			}
			if ('\0' == *plus_or_minus) {
				// No timezone, add '\0' to the beginning of microseconds so strptime doesn't have to re-parse
				// microseconds
				*orig_micro_second_str_ptr = '\0';
			} else {
				// Copy timezone info in place of microseconds
				int length = strlen(plus_or_minus);
				assert(10 > length);
				char time_zone[9];
				memcpy(time_zone, plus_or_minus, length); // Avoids memcpy-param-overlap
				memcpy(orig_micro_second_str_ptr, time_zone, length);
				*(orig_micro_second_str_ptr + length) = '\0';
				time_zone_set = TRUE;
			}
		}
	}
	// At this point the time_str will only have date time and timezone, microseconds is extracted to micro

	// Change micro_second_str_ptr to a different name
	micro_second_str_ptr = strptime(time_str, time_format, &tm1);
	boolean_t format_match = FALSE;
	char	  plus_or_minus_offset;
	int	  timezone_seconds = 0;
	if (NULL == micro_second_str_ptr) {
		// Format mis-match, continue and process what has matched alone
	} else if ('\0' == *micro_second_str_ptr) {
		// There is no microseconds
		// or
		// There is perfect match between format and value. In this case __gmt_off will be set
		format_match = TRUE;
		if (include_time_zone) {
			time_zone_set = TRUE;
		}
	} else {
		// Following query can reach here, no additional processing needed
		// select timestamp'11-07-2023 22:30:00+0530';
		if (include_time_zone) {
			if (':' == *micro_second_str_ptr) {
				timezone_seconds = atoi(micro_second_str_ptr + 1);
				if (0 > tm1.__tm_gmtoff) {
					timezone_seconds = -timezone_seconds;
				}
				time_zone_set = TRUE;
				format_match = TRUE; // This conveys that the timezone information is fully processed
			}
		}
	}
	boolean_t tm_sec_modified = FALSE;
	if (time_zone_set) {
		if (format_match) {
			tm1.tm_sec += (-1 * (tm1.__tm_gmtoff + timezone_seconds)); // Given time zone to UTC
			tm_sec_modified = TRUE;
		} else {
			if (include_time_zone) {
				/* At this time if time zone is given we expect it to be in the format accepted by strptime */
				assert(FALSE);
			}
		}
	} else {
		/* Time zone information is not included in the value passed or we are processing a `time without time zone`.
		 * In both cases no need to do time zone related processing.
		 */
	}
	if (include_time_zone && !tm_sec_modified) {
		int gmtoff;
		if (0 == tm1.tm_mday) {
			// No date is given this is a time with time zone
			struct tm *tm2;
			// Get current date
			time_t current_time;
			current_time = time(NULL);
			// Get struct tm
			tm2 = localtime(&current_time);
			gmtoff = (-1) * tm2->__tm_gmtoff;
		} else {
			/* We need to allow mktime() to update time in tm1, otherwise daylight savings related changes will not
			 * reflect correctly.
			 * `select timestamp with time zone'2024-03-10 02:00:00';`
			 * will return `2024-03-10 01:00:00-05` instead of `2024-03-10 03:00:00-04`.
			 */
			tm1.tm_isdst = -1;
			mktime(&tm1);
			gmtoff = (-1) * tm1.__tm_gmtoff;
		}
		tm1.tm_sec += gmtoff; // Local time zone to UTC
	}

	time_t op1_time = utc_mktime(&tm1);

	// Convert op1_time to string
	ydb_string_t *ret;
	// This allocation will be freed not in Octo but by YottaDB after the external call returns.
	ret = ydb_malloc(sizeof(ydb_string_t));
	ret->address = ydb_malloc(sizeof(char) * 19); // Null terminator included

	if (0 > op1_time) {
		int dummy_var = 0; // Avoids `lvalue required as left operand of assignment` error
		NINES_COMPLIMENT(dummy_var, micro);
	}
	ret->length = sprintf(ret->address, "%ld%s", op1_time, micro);
	ret->address[ret->length] = '\0';
	ydb_free(time_format);
	ydb_free(time_str);
	return ret;
}

/* This function converts the string `value` to the date/time type in `output_type`.
 * Returns -1 on not being able to convert.
 * Returns date/time in internal format.
 */
ydb_string_t *ydboctoDateTimeStringCastC(int count, ydb_string_t *value, ydb_string_t *value_format,
					 ydb_int_t date_time_output_type) {
	UNUSED(count);
	// Get the value
	return ydboctoText2InternalFormatC(2, value, value_format);
}

// This function processes date - integer and date - date. In the first case date is returned and # of days
// in the second case.
ydb_long_t ydboctoSubDateC(int count, ydb_long_t op1, ydb_long_t op2, ydb_int_t is_op2_integer) {
	UNUSED(count);
	// micro = last 6 digits
	long int micro_op1;
	REMOVE_MICRO_SECONDS(op1, micro_op1);
	assert(0 == micro_op1);
	// Integer subtraction with date
	ydb_long_t ret;
	if (is_op2_integer) {
		// Convert op1 to tm structure
		struct tm  tm1;
		struct tm *static_tm;
		static_tm = gmtime(&op1);
		// static_tm = localtime(&op1);
		memcpy(&tm1, static_tm, sizeof(struct tm));
		// Get integer
		long int_val = op2;
		if ((INT_MAX < int_val) || (INT_MIN > int_val)) {
			return DATE_TIME_ERROR_RETURN;
		}
		// Subtract op2 to tm_days
		tm1.tm_mday -= (int)int_val;
		ret = utc_mktime(&tm1);
		ADD_MICRO_SECONDS(ret, micro_op1);
		// Validate result
		if ((ret > MAX_DATE_TIME_INTERNAL_FORMAT_VALUE) || (ret < MIN_DATE_TIME_INTERNAL_FORMAT_VALUE)) {
			return DATE_TIME_ERROR_RETURN;
		}
		return ret;
	}
	long int micro_op2;
	REMOVE_MICRO_SECONDS(op2, micro_op2);
	assert(0 == micro_op2);
	// Date - Date
	// Convert op2 to tm structure
	long int result = op1 - op2;
	// Divide by 60 to get minuts
	// Divide by 60 to get hours
	// divide by 24 to get days
	result = (((result / 60) / 60) / 24);
	ret = (long)result;
	// Validate result
	if ((ret > MAX_DATE_TIME_INTERNAL_FORMAT_VALUE) || (ret < MIN_DATE_TIME_INTERNAL_FORMAT_VALUE)) {
		return DATE_TIME_ERROR_RETURN;
	}
	return ret;
}

// Following function processes `date -time`, `timestamp with time zone - time`, `timestamp - time` and `time with time zone -
// time`. For convenience op1 is expected to be date or timestamp with time zone or timestamp or time with time zone. Returns a
// timestamp or time with time zone value in internal format.
ydb_long_t ydboctoSubDateTimeC(int count, ydb_long_t op1, ydb_int_t op1_type, ydb_long_t op2, ydb_int_t op2_type) {
	UNUSED(count);
	boolean_t convert_to_utc = FALSE;
	int	  gmtoff;
	assert((DATE_LITERAL == op1_type) || (TIMESTAMP_LITERAL == op1_type) || (TIME_WITH_TIME_ZONE_LITERAL == op1_type)
	       || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == op1_type));
	assert(TIME_LITERAL == op2_type);
	// Process op1
	// micro = last 6 digits
	long int micro_op1;
	REMOVE_MICRO_SECONDS(op1, micro_op1);
	// Prepare the tm structure
	struct tm *static_tm;
	if ((TIMESTAMP_WITH_TIME_ZONE_LITERAL == op1_type) || (TIME_WITH_TIME_ZONE_LITERAL == op1_type)) {
		static_tm = localtime(&op1);
		convert_to_utc = TRUE;
		gmtoff = static_tm->__tm_gmtoff;
	} else {
		static_tm = gmtime(&op1);
	}
	struct tm tm1;
	memcpy(&tm1, static_tm, sizeof(struct tm));
	// Process op2
	// Prepare the tm structure
	long int micro_op2;
	REMOVE_MICRO_SECONDS(op2, micro_op2);
	struct tm tm2;
	if ((TIMESTAMP_WITH_TIME_ZONE_LITERAL == op2_type) || (TIME_WITH_TIME_ZONE_LITERAL == op2_type)) {
		static_tm = localtime(&op2);
		convert_to_utc = TRUE;
		gmtoff = static_tm->__tm_gmtoff;
	} else {
		static_tm = gmtime(&op2);
	}
	memcpy(&tm2, static_tm, sizeof(struct tm));
	// Subtract op1 and op2
	// Timestamp - time
	// Time with time zone - time
	tm1.tm_hour -= tm2.tm_hour;
	tm1.tm_min -= tm2.tm_min;
	tm1.tm_sec -= tm2.tm_sec;

	// subtract the microseconds part
	micro_op1 -= micro_op2;
	float micro_op_result = (float)micro_op1 / 1000000;
	// + - + -> value has to be > 0  and < 1 (left greater)
	// + - + -> value has to be > -1 and < 0 (right greater)
	// - - + -> value has to be > -1.999998 and < 0 (left greater)
	// - - + -> value has to be > 0 and < 1 (right greater)
	// - - - -> value has to be > -1.999998 and < 0 (doesn't matter which is greater)
	if ((0 > micro_op_result) && (-1 > micro_op_result)) {
		tm1.tm_sec -= 1;
		micro_op1 += 1000000;
	}
	if (convert_to_utc) {
		tm1.tm_sec += (-1 * gmtoff);
	}
	ydb_long_t ret;
	ret = utc_mktime(&tm1);
	if ((0 > ret) && (0 > micro_op1)) {
		/* Following queries can have micro_op1 -ve its okay to ignore the negative
		 * sign as it has already been factored in to ret by the above code.
		 *   select time with time zone'00:00:00' - time'00:00:00.1';
		 *   select timestamp'1969-12-31 23:59:59' - time'01:01:01.1';
		 */
		micro_op1 = -micro_op1;
	}
	ADD_MICRO_SECONDS(ret, micro_op1);
	// Validate result
	if ((ret > MAX_DATE_TIME_INTERNAL_FORMAT_VALUE) || (ret < MIN_DATE_TIME_INTERNAL_FORMAT_VALUE)) {
		return DATE_TIME_ERROR_RETURN;
	}
	return ret;
}

/* Following function assumes `op1` is always `date` or `timestamp` or `time with time zone` and `op2` is `time` or `integer`.
 * Since the operation is commutative this assumption will still lead to a valid expression result.
 */
ydb_long_t ydboctoAddDateTimeC(int count, ydb_long_t op1, ydb_int_t op1_type, ydb_long_t op2, ydb_int_t op2_type) {
	ydb_long_t ret;
	UNUSED(count);
	boolean_t convert_to_utc = FALSE;
	int	  gmtoff;
	// Process op1
	// Prepare the tm structure
	long int micro_op1;
	REMOVE_MICRO_SECONDS(op1, micro_op1);
	// Convert op1 to tm structure
	struct tm *static_tm;
	struct tm  tm1;
	if ((TIMESTAMP_WITH_TIME_ZONE_LITERAL == op1_type) || (TIME_WITH_TIME_ZONE_LITERAL == op1_type)) {
		static_tm = localtime(&op1);
		convert_to_utc = TRUE;
		gmtoff = static_tm->__tm_gmtoff;
	} else {
		static_tm = gmtime(&op1);
	}

	memcpy(&tm1, static_tm, sizeof(struct tm));
	// Process op2
	// Integer addition to date
	if (INTEGER_LITERAL == op2_type) {
		// Get integer
		long int_val = op2;
		if ((INT_MAX < int_val) || (INT_MIN > int_val)) {
			return DATE_TIME_ERROR_RETURN;
		}
		// Add op2 to tm_days
		tm1.tm_mday += (int)int_val;
		ret = utc_mktime(&tm1);
		ADD_MICRO_SECONDS(ret, micro_op1);
		// Validate result
		if ((ret > MAX_DATE_TIME_INTERNAL_FORMAT_VALUE) || (ret < MIN_DATE_TIME_INTERNAL_FORMAT_VALUE)) {
			return DATE_TIME_ERROR_RETURN;
		}
		return ret;
	}
	long int micro_op2;
	REMOVE_MICRO_SECONDS(op2, micro_op2);
	// Prepare the tm structure
	struct tm tm2;
	if ((TIMESTAMP_WITH_TIME_ZONE_LITERAL == op2_type) || (TIME_WITH_TIME_ZONE_LITERAL == op2_type)) {
		static_tm = localtime(&op2);
		convert_to_utc = TRUE;
		gmtoff = static_tm->__tm_gmtoff;
	} else {
		static_tm = gmtime(&op2);
	}
	memcpy(&tm2, static_tm, sizeof(struct tm));
	// Add op1 and op2
	tm1.tm_hour += tm2.tm_hour;
	tm1.tm_min += tm2.tm_min;
	tm1.tm_sec += tm2.tm_sec;
	// micro sec addition
	micro_op1 += micro_op2;
	float micro_op_result = (float)micro_op1 / 1000000;
	assert(1.999999 > micro_op_result);
	if (1.0 <= micro_op_result) {
		tm1.tm_sec += 1;
		micro_op1 -= 1000000;
	}
	if (convert_to_utc) {
		tm1.tm_sec += (-1 * gmtoff);
	}
	ret = utc_mktime(&tm1);
	ADD_MICRO_SECONDS(ret, micro_op1);
	// Validate result
	if ((ret > MAX_DATE_TIME_INTERNAL_FORMAT_VALUE) || (ret < MIN_DATE_TIME_INTERNAL_FORMAT_VALUE)) {
		return DATE_TIME_ERROR_RETURN;
	}
	// Return the result
	return ret;
}

/* Validate date/time value of any format
 * Returns:
 * 	0 on success
 *	1 on error
 */
ydb_int_t ydboctoValidateDateTimeValueC(int count, ydb_string_t *value, ydb_int_t value_type, ydb_int_t value_format,
					ydb_string_t *text_format) {
	// make a copy of the ydb_string_t to a char* value_str
	char *orig_value_str;
	char *value_str = ydb_malloc(value->length + 1);
	memcpy(value_str, value->address, value->length);
	value_str[value->length] = '\0';
	orig_value_str = value_str;
	char	       *text_format_str = NULL;
	SqlValueType	vt = value_type;
	OptionalKeyword vf = value_format;
	if (OPTIONAL_DATE_TIME_TEXT == vf) {
		text_format_str = ydb_malloc(text_format->length + 1);
		memcpy(text_format_str, text_format->address, text_format->length);
		text_format_str[text_format->length] = '\0';
	}
	int ret = validate_date_time_value(&value_str, vt, vf, text_format_str);
	ydb_free(orig_value_str);
	if (NULL != text_format_str) {
		ydb_free(text_format_str);
	}
	return ret;
}
