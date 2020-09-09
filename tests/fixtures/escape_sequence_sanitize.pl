#################################################################
#								#
# Copyright (c) 2017-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
#
# --------------------------------------------------------------------------------
# Note: This script is copied over from the YDBTest repo `com/expectsanitize.pl`.
# --------------------------------------------------------------------------------
#
# This perl script removes control characters from an expect session. A few portions of it is copied from
#	https://unix.stackexchange.com/questions/14684/removing-control-chars-including-console-codes-colours-from-script-output
#
# Removing the control characters makes it possible to paste the entire expect output in the test reference file.
#
#################################################################

while (<>) {
    s/ (?:\e\[|\x9b) [ -?]* [@-~] | \e.|[\x80-\x9f] |[\b]|[\r]//xg;
    print;
}

