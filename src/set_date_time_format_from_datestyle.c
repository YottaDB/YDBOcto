/****************************************************************
 *								*
 * Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "octo.h"
#include "octo_types.h"

// 0 for success
// 1 for error
/* Currently Octo only supports ISO and any combination of MDY chars */
int set_date_time_format_from_datestyle(char *date_style_value) {
	char *date_style_value_str = date_style_value; // To print out the entire value in error message
	if (NULL == date_style_value) {
		// Invalid input
		ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
		return 1;
	}
	if (0 == strlen(date_style_value)) {
		// Nothing to set
		return 0;
	}
	char *date_format = malloc(strlen(DEFAULT_DATE_FORMAT) + 1);
	char *timestamp_format = malloc(strlen(DEFAULT_TIMESTAMP_FORMAT) + 1);
	char *date_style_config_value_str = malloc(strlen(DEFAULT_DATESTYLE) + 1); // ISO, MDY
	// Even if not specified ensure ISO is part of datestyle value as it is the only output form Octo currently implements
	strcpy(date_style_config_value_str, "ISO, ");
	int datestyle_length = 5; // 'ISO, '
	int date_format_index = 0;
	int timestamp_format_index = 0;
	TOLOWER_STR(date_style_value);
	boolean_t is_iso_format = FALSE;
	boolean_t is_first_char = TRUE;
	boolean_t is_date_time_format_set = FALSE;
	boolean_t is_separator_reached = FALSE;
	boolean_t is_year_set = FALSE;
	boolean_t is_month_set = FALSE;
	boolean_t is_day_set = FALSE;
	char	  ch;
	while ('\0' != *date_style_value) {
		ch = *date_style_value;
		switch (ch) {
		case 'i':
			if (is_iso_format) {
				// Already parsed 'i', this is a repeated occurence
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			}
			if (('\0' == *(date_style_value + 1)) || ('\0' == *(date_style_value + 2))) {
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			}
			if (('s' == *(date_style_value + 1)) && ('o' == *(date_style_value + 2))) {
				is_iso_format = TRUE;
			} else {
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			}
			date_style_value += 2;
			break;
		case 'y':
		case 'm':
		case 'd':
			if (is_date_time_format_set) {
				// Already parsed, this is a repeated occurence
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			}
			is_date_time_format_set = TRUE;
			int i = 0;
			do {
				if (('y' == ch) || ('m' == ch) || ('d' == ch)) {
					boolean_t is_error = FALSE;
					if ('y' == ch) {
						if (is_year_set) {
							is_error = TRUE;
						}
						is_year_set = TRUE;
					} else if ('m' == ch) {
						if (is_month_set) {
							is_error = TRUE;
						}
						is_month_set = TRUE;
					} else if ('d' == ch) {
						if (is_day_set) {
							is_error = TRUE;
						}
						is_day_set = TRUE;
					}
					if (is_error) {
						// ERROR
						free(date_format);
						free(timestamp_format);
						free(date_style_config_value_str);
						ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
						return 1;
					} else {
						// Do the addition here
						if (!is_first_char) {
							date_format[date_format_index++] = '-';
							timestamp_format[timestamp_format_index++] = '-';
						} else {
							is_first_char = FALSE;
						}
						// concat the format specifier to dateformatstring
						date_format[date_format_index++] = '%';
						timestamp_format[timestamp_format_index++] = '%';
						char value = ('y' == ch) ? 'Y' : ch;
						date_format[date_format_index++] = value;
						timestamp_format[timestamp_format_index++] = value;
						date_style_config_value_str[datestyle_length++] = toupper(ch);
					}
				} else {
					// ERROR
					free(date_format);
					free(timestamp_format);
					free(date_style_config_value_str);
					ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
					return 1;
				}
				i++;
				ch = *(date_style_value + i);
			} while (i < 3);
			date_style_value += 2;
			break;
		case ',':
			if (is_separator_reached) {
				// ,, -> ERROR
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			} else if (is_iso_format && is_date_time_format_set) {
				// ISOMDY, -> ERROR
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			} else if (!is_iso_format && !is_date_time_format_set) {
				// ,ISOMDY -> ERROR
				free(date_format);
				free(timestamp_format);
				free(date_style_config_value_str);
				ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
				return 1;
			}
			// This is a valid separator ignore it
			is_separator_reached = TRUE;
			break;
		case ' ':
		case '\t':
		case '\n':
			// ignore white space
			break;
		default:
			// un-expected character reached
			free(date_format);
			free(timestamp_format);
			free(date_style_config_value_str);
			ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
			return 1;
			break;
		}
		date_style_value += 1;
	}
	if (is_separator_reached && (!is_iso_format || !is_date_time_format_set)) {
		// 'mdy,', 'iso,',',iso' -> ERROR
		free(date_format);
		free(timestamp_format);
		free(date_style_config_value_str);
		ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
		return 1;
	} else if (is_iso_format && is_date_time_format_set && !is_separator_reached) {
		// 'mdyiso','isomdy' -> ERROR
		free(date_format);
		free(timestamp_format);
		free(date_style_config_value_str);
		ERROR(ERR_INVALID_DATESTYLE_VALUE, date_style_value_str);
		return 1;
	}
	if (is_iso_format) {
		// Output formats are constant so ignore it for now
	}
	if (is_date_time_format_set) {
		if (NULL != config->datestyle) {
			free((char *)config->datestyle);
		}
		if (NULL != config->date_format) {
			free((char *)config->date_format);
		}
		if (NULL != config->timestamp_format) {
			free((char *)config->timestamp_format);
		}
		if (NULL != config->timestamptz_format) {
			free((char *)config->timestamptz_format);
		}
		date_style_config_value_str[datestyle_length] = '\0';
		config->datestyle = date_style_config_value_str;
		date_format[date_format_index] = '\0';
		config->date_format = date_format;
		timestamp_format[timestamp_format_index++] = ' ';
		strcpy(&timestamp_format[timestamp_format_index], DEFAULT_TIME_FORMAT);
		config->timestamp_format = timestamp_format;

		char *timestamptz_format = malloc(strlen(DEFAULT_TIMESTAMP_FORMAT) + 1 + 2);
		strcpy(timestamptz_format, timestamp_format);
		int len = strlen(timestamptz_format);
		timestamptz_format[len] = '%';
		timestamptz_format[len + 1] = 'z';
		timestamptz_format[len + 2] = '\0';
		config->timestamptz_format = timestamptz_format;
	} else {
		free(date_format);
		free(timestamp_format);
		free(date_style_config_value_str);
	}
	return 0;
}
