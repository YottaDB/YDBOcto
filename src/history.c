/****************************************************************
 * 								*
 * Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 * 								*
 * This source code contains the intellectual property		*
 * of its copyright holder(s), and is made available		*
 * under a license.  If you do not know the terms of		*
 * the license, please stop and do not read further.		*
 * 								*
 * Portions Copyright (c) 2000-2021, PostgreSQL Global 		*
 * Development Group 						*
 * BSD style license here: 					*
 * https://github.com/postgres/postgres/blob/master/COPYRIGHT	*
 * **************************************************************/

#include <fcntl.h>   // for open() call
#include <wordexp.h> // to expand tilde (~)
#include <limits.h>  // NAME_MAX, PATH_MAX
#include <assert.h>  // assert
#include <stdio.h>   // For FILE, as readline doesn't include it. CentOS issue.

// readline lib stuff
#include <readline.h>
#include <readline/history.h>

static int history_lines_added;

#include "octo.h"

/* Global config variables used throughout:
 * config->octo_history:
 * 1. Contains initial unexpanded value of history location (e.g. ~/.octo_history)
 * 2. After call to wordexp(3), either:
 * - Expanded value of history location (e.g. /home/sam/.octo_history)
 * - NULL, if wordexp failed.
 *
 * config->octo_history_max_length: History entries limit.
 *
 */

/* Ensure that file exists prior to reading/writing history, otherwise history
 * reading/writing fails
 * NB: Private function to this file
 */
boolean_t ensure_file_exists(void) {
	int fd;

	fd = open(config->octo_history, O_CREAT, S_IRUSR | S_IWUSR);
	if (fd >= 0) {
		close(fd);
		return TRUE;
	}
	if (fd < 0) {
		return FALSE;
	}
	return FALSE;
}

/* encode_history, decode_history copied and simplified from
 * https://github.com/postgres/postgres/blob/master/src/bin/psql/input.c
 *
 * It's simplified as we only support readline, not libedit, which has
 * different semantics. I also folded them into one function rather than two.
 *
 * psql comment: Convert newlines to NL_IN_HISTORY for safe saving in readline
 * history file.
 * NB: Private function to this file
 */
#define NL_IN_HISTORY 0x01
enum EncodeDecodeEnum { ENCODE, DECODE };
void encode_decode_history(enum EncodeDecodeEnum ed) {
	HIST_ENTRY *h;
	char	   *c;

	history_set_pos(0);
	for (h = current_history(); NULL != h; h = next_history()) {
		for (c = h->line; *c; c++) {
			if (ed == ENCODE && *c == '\n')
				*c = NL_IN_HISTORY;
			if (ed == DECODE && *c == NL_IN_HISTORY)
				*c = '\n';
		}
	}
}

/* Load history for readline history file in config->octo_history */
void load_readline_history(void) {
	int result;

	// We couldn't resolve the file, so quit
	if (NULL == config->octo_history)
		return;

	// No point in reading if the history length is zero
	if (0 == config->octo_history_max_length)
		return;

	INFO(INFO_READLINE_NOTIFY_LOAD, "");

	/* Create file so that the very first read_history call doesn't fail when
	 * the file doesn't exist yet (first time we run Octo)
	 */
	if (!ensure_file_exists()) {
		WARNING(WARN_READLINE_LOAD_FAIL, config->octo_history);
		return;
	}

	// Actual call to libreadline to load history
	result = read_history(config->octo_history);
	if (0 != result) {
		WARNING(WARN_READLINE_LOAD_FAIL, config->octo_history);
		return;
	}

	// Decode newlines from history
	encode_decode_history(DECODE);
	INFO(INFO_READLINE_NOTIFY_LOAD_COUNT, history_length);
}

/* Save history into history file in config->octo_history, trimming as needed */
void save_readline_history(void) {
	int result;

	// We couldn't resolve the file, so quit
	if (NULL == config->octo_history)
		return;

	// We shouldn't store anything as octo_history_max_length is zero
	if (0 == config->octo_history_max_length)
		return;

	INFO(INFO_READLINE_NOTIFY_SAVE, "");

	/* There used to be a call here to create the file if it doesn't exist,
	 * but the call to load history will create it, so won't repeat here.
	 */

	// Clamp the number of lines we will add to history to the history max length
	if (history_lines_added > config->octo_history_max_length)
		history_lines_added = config->octo_history_max_length;

	/* If we are stifled from `history-size` in the INPUTRC, clamp to history_max_entries;
	 * We won't have the added lines in history if we are stilfed, so we can only save history_max_entries
	 */
	if ((0 != history_max_entries) && (history_lines_added > history_max_entries))
		history_lines_added = history_max_entries;

	/* Truncate history file so that history_initial_length + history_lines_added = config->octo_history_max_length.
	 * Previously, we notified the user of the results of the truncation operation (how many will be discarded),
	 * but I found out that this is not something that Readline supports instrinsically; and if `history-size` is set
	 * in INPUTRC, there is no way to perform the calculation of how many previous entries there were and how many
	 * will be discarded. Therefore, the previous message is now not shown.
	 */
	history_truncate_file(config->octo_history, config->octo_history_max_length - history_lines_added);
	INFO(INFO_READLINE_NOTIFY_SAVE_COUNT, history_lines_added);

	// If nothing to append, return
	if (0 == history_lines_added)
		return;

	/* Shrink down the history buffer to the entries we are gonna save before
	 * we encode/decode them so we don't encode entries we aren't gonna save.
	 */
	stifle_history(history_lines_added);

	// encode new lines into history
	encode_decode_history(ENCODE);

	// Now append the current session's history
	result = append_history(history_lines_added, config->octo_history);
	if (0 != result) {
		WARNING(WARN_READLINE_SAVE_FAIL, config->octo_history);
		return;
	}

	/* We are done with history processing. Free memory allocated to
	 * config->octo_history (calloc in set_readline_file)
	 */
	free((void *)config->octo_history);
}

/* Return readline history file into variable config->octo_history.
 * config->octo_history may contain a value already, we may expand it and store
 * back the expanded value. If expansion fails, set it to NULL.
 */
void set_readline_file(void) {
	const char  *readline_initialfile = NULL;
	wordexp_t    wordexp_result;
	char	    *readline_actualfile;
	unsigned int readline_actualfile_max_length;
	int	     wordexp_status;

	// Final History File memory location. Allocate memory for it and zero out
	// free() in save_readline_history()
	readline_actualfile_max_length = PATH_MAX + NAME_MAX + 1; // 1 for NULL
	readline_actualfile = calloc(sizeof(char), readline_actualfile_max_length);

	/* Get history file
	 * First from config file. If not present, default to ~/.octo_history
	 */
	if (config->octo_history)
		readline_initialfile = config->octo_history;
	if (NULL == readline_initialfile)
		readline_initialfile = OCTO_HISTORY_DEFAULT;

	/* Expand the tilde.
	 * The nasty loop is to deal with spaces.
	 * wordexp behaves like argc and argv, with c providing the number of
	 * arguments into v.
	 */
	wordexp_status = wordexp(readline_initialfile, &wordexp_result, 0);
	if (0 == wordexp_status) {
		unsigned int offset;

		offset = 0; // offset into readline_actualfile char array
		for (unsigned int i = 0; i < wordexp_result.we_wordc; i++) {
			char	    *token;
			unsigned int token_length;

			/* Multiple tokens means that there is a space
			 * separating the tokens. Therefore, add it here.
			 * First one (i = 0) is the initial, and we don't add a
			 * space in case it's just the only one. I.e., we are
			 * adding a space AFTER the initial element, because we
			 * will loop again only because a space comes before
			 * our entry.
			 */
			if (i > 0)
				readline_actualfile[offset++] = ' ';

			token = wordexp_result.we_wordv[i];
			token_length = strlen(token);

			/* Prevent overflow -- this just means somebody is playing to crash
			 * this. Warn and exit.
			 * +2 for space and NULL.
			 */
			if (offset + token_length + 2 > readline_actualfile_max_length) {
				WARNING(WARN_READLINE_LOAD_FAIL, readline_initialfile);
				// Clear output
				readline_actualfile[0] = '\0';
				break;
			}

			memcpy(readline_actualfile + offset, token, token_length);
			offset += token_length;
		}
		readline_actualfile[offset] = '\0';
	} else {
		WARNING(WARN_READLINE_LOAD_FAIL, readline_initialfile);
	}
	if ((0 == wordexp_status) || (WRDE_NOSPACE == wordexp_status)) {
		wordfree(&wordexp_result);
	}
	// Final answer!
	if (readline_actualfile[0] == '\0') {
		config->octo_history = NULL;
		free(readline_actualfile);
	} else {
		config->octo_history = readline_actualfile;
		INFO(INFO_READLINE_NOTIFY_HIST_LOCATION, readline_actualfile);
	}
}

/* Set maximum history length based on config file. */
void set_octo_history_max_length(void) {
	/* NB:  psql does not stifle_history at runtime, so I won't do it here.
	 * I.e., it stores as many entries in the history regardless of size while
	 * it's running. The only exception is if history-size is set in INPUTRC,
	 * and in that case, the stifling is already done when readline is initialized.
	 */

	/* Check if user actually set this or it is blank.  If blank, set to default of 500. */
	if (config->octo_history_max_length == OCTO_HISTORY_MAX_LENGTH_UNSET)
		config->octo_history_max_length = OCTO_HISTORY_MAX_LENGTH_DEFAULT;

	// Check if the user set a negative number. If so, clamp to zero
	if (config->octo_history_max_length < 0)
		config->octo_history_max_length = 0;

	INFO(INFO_READLINE_NOTIFY_HIST_COUNT, config->octo_history_max_length);

	/* If history-size is set in INPUTRC, then tell the user, as otherwise it looks rather weird
	 * that we have a 500 long line history but only show/save a small number of entries
	 */
	if (0 != history_max_entries)
		INFO(INFO_READLINE_NOTIFY_HIST_STIFLED, history_max_entries);
}

/* Implementation of \s to print history */
void print_history(void) {
	HIST_ENTRY *h;
	sigset_t    savemask;

	history_set_pos(0);

	// Block signals while printing history in case calls are interrupted by YottaDB signals
	sigprocmask(SIG_BLOCK, &block_sigsent, &savemask);
	for (h = current_history(); NULL != h; h = next_history())
		fprintf(stdout, "%s\n", h->line); // NB: already SIGNAL PROTECTED
	fflush(stdout);
	sigprocmask(SIG_SETMASK, &savemask, NULL);
}

/* Readline setup call */
void readline_setup(void) {
	/* Allow conditional parsing of the ~/.inputrc file. */
	rl_readline_name = "Octo";
	// Initialize readline (reads INPUTRC file, stifle history in advance if history-size is set, set history_max_entries var)
	rl_initialize();
	// Turn on history
	using_history();
	// Set to zero (but static var, so already zero)
	history_lines_added = 0;
	// display the tab_completion of '\t' and just insert it as a character
	rl_bind_key('\t', rl_insert);
	// disable bracketed paste so that cursor doesn't jump to beginning of prompt
	rl_variable_bind("enable-bracketed-paste", "off");
	set_readline_file();
	set_octo_history_max_length();
	load_readline_history();
}

/* Add single history item to readline history but don't duplicate */
void add_single_history_item(char *input_buffer_combined, int old_input_index) {
	HIST_ENTRY *cur_hist;

	/* get the last item added to the history
	 * if it is the same as the current query don't add it to the history again
	 * Count lines added to history for use later in save_readline_history *
	 */
	cur_hist = history_get(history_base + history_length - 1);
	if (NULL != cur_hist) {
		if (0 != strcmp(cur_hist->line, input_buffer_combined + old_input_index)) {
			add_history(input_buffer_combined + old_input_index);
			history_lines_added++;
		}
	} else {
		add_history(input_buffer_combined + old_input_index);
		history_lines_added++;
	}
}
