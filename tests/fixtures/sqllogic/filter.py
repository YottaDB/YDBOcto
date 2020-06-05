#!/usr/bin/env python
"""
#################################################################
#                                                               #
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

Given a file from https://github.com/shivarajugowda/jdbcSQLTest/tree/master/resources/sqllogictest
with many queries, generate a new file for each query.
Each file will also include the original description of the test.
"""

import sys
import os
import random
from os import path

print("generating all queries")

file_num = int(sys.argv[1])
percentage = float(sys.argv[2]) if len(sys.argv) > 2 else 1

# Open the input relative to the python script, not the current working directory
root = path.dirname(path.abspath(__file__))
input_filename = path.join(root, 'select{}.test'.format(file_num))

current_query = []
description = ''
queries = []
template = """-- Derived from https://github.com/shivarajugowda/jdbcSQLTest/tree/master/resources/sqllogictest
--{}
{};
"""


with open(input_filename) as query_file:
    for i, line in enumerate(query_file):
        # End of an ongoing query
        if "----" in line:
            assert current_query, "saw end of a query, but there was no query ongoing (on line {})".format(i)
            queries.append(current_query)
            current_query = []
            description = ''
        # Start of a new query
        elif line.strip().startswith("query"):
            description = line.strip()[len("query"):]
            assert description, "queries should have a description"
            assert not current_query, "saw the start of a new query, but the old one didn't finish (on line {})".format(i)
        # Existing query
        elif description:
            current_query.append(line)
        else:
            # probably a blank line or something
            pass

num_to_output = int(round(len(queries) * percentage))
# Keep the original index of the query even though we're only taking a subset
outputs = random.sample(list(enumerate(queries)), num_to_output)
for query_num, query in outputs:
    filename = "test{}.sql".format(query_num)
    with open(filename, 'w') as fd:
        content = template.format(description, ''.join(query))
        fd.write(content)

print("all done! generated {} out of {} available queries".format(num_to_output, len(queries)))
