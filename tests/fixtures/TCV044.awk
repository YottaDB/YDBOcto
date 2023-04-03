#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Helper awk program to generate a list of CREATE VIEW commands where the underlying SELECT query
# for the VIEW is copied from the list of SELECT queries passed in stdin.

{
	gsub(/;$/,"")
	printf "CREATE VIEW tcv044_%d as %s;\n", NR, $0;
	printf "\\d tcv044_%d;\n", NR;
	printf "select * from tcv044_%d limit 1;\n", NR;

	newquery = $0;
	srcview = sprintf("tcv044_%d", NR);
	dstview = sprintf("tcv044_%dd", NR);
	gsub(/\<names\>/,srcview,newquery);
	gsub(/\<NAMES\>/,srcview,newquery);
	printf "CREATE VIEW %s as %s;\n", dstview, newquery;
	printf "\\d %s;\n", dstview;
	printf "select count(*) as \"count_%s\" from %s;\n", dstview, dstview;

	newquery = $0;
	srcview = sprintf("tcv044_%dd", NR);
	dstview = sprintf("tcv044_%de", NR);
	gsub(/\<names\>/,srcview,newquery);
	gsub(/\<NAMES\>/,srcview,newquery);
	printf "CREATE VIEW %s as %s;\n", dstview, newquery;
	printf "\\d %s;\n", dstview;
	printf "select count(*) as \"count_%s\" from %s;\n", dstview, dstview;

	printf "\n";
}

