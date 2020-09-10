#!/bin/bash
#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Note that this file is similar to doc_error_check.sh, so changes there may also need to be made here.
missing_errors=""
while read line; do
	mnemonic=$(echo $line | sed 's/ERROR_DEF(\(ERR_.*\|INFO_.*\),/\1/' | cut -f 1 -d ',')
	if [[ $(grep -cn "^$mnemonic$" doc/errors.rst) -eq 0 ]]; then
		missing_errors="$mnemonic\n$missing_errors"
	else
		format_string=$(echo $line | sed 's/.*, \(".*"\), PSQL.*)/\1/' | sed 's/%s\|%d\|%c\|%x\|%ld\|%\.\*s/xxx/g')
		if [[ $(grep -c "$format_string" doc/errors.rst) -eq 0 ]]; then
			line_number=$(grep -n "^$mnemonic$" doc/errors.rst | cut -f 1 -d ':')
			# line_number=$(grep -n $mnemonic errors.rst)
			if [[ "" != $line_number ]]; then
				line_number=$(( $line_number+2 ))
				# Sed expects a newline here, hence the line break
				temp=$(sed -i "${line_number}a\
					$format_string\n" doc/errors.rst)
			else
				echo "-> Error message $mnemonic missing from errors.rst. Please add this mnemonic error name along with its message text, error code, and a description to doc/errors.rst."
				exit 1
			fi
		fi
	fi
	# Use redirect to run loop in main subshell to allow access to variables outside the loop
	# Also omit any mnemonics specified in omitted_errors.txt, as these are exceptional cases due to the "ROCTO_" portion of the
	# mnemonic being significant and not merely an identifying prefix, or else due to the inclusion of regex characters like '*'
	# in the error message text that interfere with the above sed calls. These omissions are acceptable since these particular
	# messages are known to be complete in the documentation.
done < <(grep ERROR_DEF src/errors.hd | grep -v -f tools/ci/omitted_errors.ref)
if [[ "" != $missing_errors ]]; then
	echo "-> The following error message mnemonics are missing from errors.rst. Please add each mnemonic error name along with its message text, error code, and a description to doc/errors.rst:"
	echo -e $missing_errors
	exit 1
fi
