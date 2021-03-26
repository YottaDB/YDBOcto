#!/bin/bash
#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

set -e

topleveldir=$(git rev-parse --show-toplevel)
cwd=$(pwd)
if [[ $topleveldir != "$cwd" ]]; then
	echo "ERROR: doc_error_update.sh run from $cwd (expected: $topleveldir)"
	exit 1
fi

missing_mnemonics=""
missing_messages=""
missing_text=""
duplicated_text=""
while read -r line; do
	mnemonic=$(echo "$line" | sed 's/ERROR_DEF(\(ERR_.*\|INFO_.*\|WARN_.*\),/\1/' | cut -f 1 -d ',')
	if [[ $(grep -cn "^$mnemonic$" doc/errors.rst) -eq 0 ]]; then
		missing_mnemonics="$mnemonic\n$missing_mnemonics"
	else
		format_string=$(echo "$line" | sed 's/.*, "\(.*\)", PSQL.*)/\1/' | sed 's/%s\|%d\|%c\|%x\|%ld\|%\.\*s/xxx/g')
		if [[ $1 == "check" ]]; then
			if [[ $(grep -c "$format_string" doc/errors.rst) -eq 0 ]]; then
				missing_messages="$mnemonic\n$missing_messages"
			fi
		else
			format_occurrences=$(grep -cn "^Text: $format_string$" doc/errors.rst)
			if [[ $format_occurrences -ne 1 ]]; then
				if [[ $format_occurrences -eq 0 ]]; then
					missing_text="$mnemonic\n$missing_text"
					line_number=$(grep -n "^$mnemonic$" doc/errors.rst | cut -f 1 -d ':')
					# line_number=$(grep -n $mnemonic errors.rst)
					if [[ "" != "$line_number" ]]; then
						line_number=$(( line_number+2 ))
						# Sed expects a newline here, hence the line break
						sed -i "${line_number}a\
							Text: $format_string\n" doc/errors.rst
					else
						echo "-> Error message $mnemonic missing from errors.rst. Please add this mnemonic error name along with its message text, error code, and a description to doc/errors.rst."
						exit 1
					fi
				else
					duplicated_text="$mnemonic\n$duplicated_text"
				fi
			fi
		fi
	fi
	# Use redirect to run loop in main subshell to allow access to variables outside the loop
	# Also omit any mnemonics specified in omitted_errors.txt, as these are exceptional cases due to the "ROCTO_" portion of the
	# mnemonic being significant and not merely an identifying prefix, or else due to the inclusion of regex characters like '*'
	# in the error message text that interfere with the above sed calls. These omissions are acceptable since these particular
	# messages are known to be complete in the documentation.
	# Also ignore comment lines (that start with a '//' pattern). This way if an error message gets added
	# but is inside a commented line, the pre-commit hook does not fail due to that commented error message
	# not being currently documented in "doc/errors.rst". Hence the grep for '^//' below.
done < <(grep ERROR_DEF src/errors.hd | grep -v '^//' | grep -v -f tools/ci/omitted_errors.ref)
result=0
if [[ "" != "$missing_mnemonics" ]]; then
	echo "-> The following error message mnemonics are missing from errors.rst. Please add each mnemonic error name along with its message text, error code, and a description to doc/errors.rst:"
	echo -e "$missing_mnemonics"
	result=1
fi
if [[ $1 == "check" ]]; then
	if [[ "" != "$missing_messages" ]]; then
		echo "-> Error message text for the following mnemonics is missing from errors.rst. Please add the error message text for each to doc/errors.rst:"
		echo -e "$missing_messages"
		result=1
	fi
	exit $result
else
	if [[ $result -eq 1 ]]; then
		# Error already message issued for this case, so just exit
		exit 1
	elif [[ "" != "$missing_text" ]]; then
		echo "-> The following error message mnemonics were found in errors.rst, but lacking error message text. This text has been automatically added for the following errors:"
		echo -e "$missing_text"
		exit 1
	elif [[ "" != "$duplicated_text" ]]; then
		echo "-> The following error message mnemonics are duplicated in errors.rst. Please remove the duplicate instance(s) of each error along with its message text, error code, and description from doc/errors.rst:"
		echo -e "$duplicated_text"
		exit 1
	fi
fi
