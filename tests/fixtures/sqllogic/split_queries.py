#!/usr/bin/env python3
"""
#################################################################
#                                                               #
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

Given an SQL file with many queries, generate a separate file for each query.
Queries can span multiple lines, but there cannot be multiple queries on the same line.
"""

import sys
from os import path
from filter import parse_queries

filename = path.abspath(sys.argv[1])

def line_info(line):
    line = line.strip()
    is_start = line and not line.startswith("--")
    # NOTE: this will need to be fixed if we ever have `SELECT ';'` or similar
    is_end = ';' in line
    # Queries are always inclusive
    inclusive = is_start or is_end
    return (is_start, is_end, inclusive)

with open(filename) as fd:
    # Allow '#' comments
    lines = filter(lambda line: not line.strip().startswith('#'), fd)
    queries = list(parse_queries(lines, line_info))

root, ext = path.splitext(filename)
digits = len(str(len(queries)))
cnt = 0
if len(sys.argv) > 2:
    query_filter = sys.argv[2]
else:
    query_filter = ""
for query in queries:
    # Check if argv[2] was provided. If yes it is a string such that only queries that contain this search string
    # are included in the final split query files. An example search string is "CREATE TABLE" or "SELECT".
    if query_filter != "":
        skip = not any(query_filter in s for s in query)
        if skip:
            continue
    with open("{}-{}{}".format(root, str(cnt).zfill(digits), ext), 'w') as output:
        output.write(''.join(query))
    cnt += 1
