#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Flag an M ELSE preceded by DO or XECUTE with an argument, on the same line or the previous same-level line.
#
# Such a call does not restore $TEST, so the ELSE can run even after a true IF. The fix is to end the line
# with "IF 1", which sets $TEST back to 1 once the call returns:
#   IF a DO foo(1) IF 1
#   ELSE  ...
# A line whose last command is "IF 1" is therefore not flagged.
#
# Matching just the line before each ELSE is not enough, because this correct form would be flagged:
#   . IF a DO
#   . . DO foo(1)
#   . ELSE  ...
# Here "DO foo(1)" runs inside the argumentless DO's dot block, which restores $TEST on exit.
# So the script tracks dot levels, and compares each ELSE only with the last line at its own level.
#
# Prints "file:line" for each offending ELSE and always exits 0, so callers test whether it printed anything.
BEGIN {
	call = "(^| )(D|DO|X|XECUTE)(:[^ ]*)? [^ ]"
	restore = "(^| )I(F)? 1$"
}
FNR == 1 { delete prev }
{
	line = $0
	gsub(/"[^"]*"/, "\"\"", line)	# blank out strings, so ";" and "DO x" inside them are ignored
	sub(/;.*/, "", line)		# strip comments
	sub(/[ \t]+$/, "", line)	# strip trailing white space, so "IF 1 ; comment" still ends in "IF 1"
	if (line ~ /^[^ \t]/)
		delete prev		# a label starts a new entry point
	sub(/^[^ \t]*[ \t]+/, "", line)	# strip label and line-start white space
	match(line, /^[. \t]*/)
	lead = substr(line, 1, RLENGTH)
	body = toupper(substr(line, RLENGTH + 1))
	if (body == "")
		next
	lvl = gsub(/\./, "", lead)
	for (k in prev)
		if (k + 0 > lvl)
			delete prev[k]
	if (body ~ /^E(LSE)? / && prev[lvl] ~ call && prev[lvl] !~ restore)
		print FILENAME ":" FNR
	else if (body ~ (call ".* E(LSE)? "))
		print FILENAME ":" FNR
	prev[lvl] = body
}
