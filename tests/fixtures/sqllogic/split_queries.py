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
import random
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
    # Filter out lines starting with '#' or '--' (comment lines). Do not want them to show up in the output *.sql files
    lines = filter(lambda line: not line.strip().startswith('#') and not line.strip().startswith('--'), fd)
    queries = parse_queries(lines, line_info)

# Check if argv[2] was provided. If yes it is a string such that only queries that contain this search string
# are included in the final split query files. An example search string is "CREATE TABLE" or "SELECT".
query_filter = sys.argv[2] if len(sys.argv) > 2 else ""

# If no fraction was specified, include all queries for splitting.
# Else include a random sample of queries based on the specified fraction.
fraction = float(sys.argv[3]) if len(sys.argv) > 3 else 1
# Below code is similar to the "write_random_sample()" function in "tests/fixtures/sqllogic/filter.py"
num_to_output = int(round(len(queries) * fraction))
# Keep the original index of the query even though we're only taking a subset
outputs = random.sample(list(enumerate(queries)), num_to_output)

root, ext = path.splitext(filename)
digits = len(str(len(queries)))
for query_num, query in outputs:
    if query_filter != "":
        skip = not any(query_filter in s for s in query)
        if skip:
            continue
    with open("{}-{}{}".format(root, str(query_num).zfill(digits), ext), 'w') as output:
        output.write(''.join(query))

