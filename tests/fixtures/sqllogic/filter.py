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

def parse_queries(file, is_start, is_end):
    """
    Parse the queries in a file.
    Parameters:
    - a `file` which iterates over lines
    - a function `is_start` that tells whether a line is the start of a query
    - a function `is_end` that tells whether a line is the end of a query
    Note that neither the start nor end lines are added to the query.
    Returns a list of queries, where each query is a list of lines.
    """
    current_query = []
    queries = []
    # The state here is a little odd because we could be in a query
    # but have `current_query` be empty if we just saw the start line.
    # Instead we have an explicit in_query variable.
    in_query = False
    for i, line in enumerate(file):
        # End of an ongoing query
        if is_end(line):
            # Ignore empty queries
            if current_query:
                queries.append(current_query)
                current_query = []
                in_query = False
        # Start of a new query
        elif is_start(line):
            assert not in_query, (
                "saw the start of a new query, but the old one didn't finish" +
                " (on line {}: {})".format(i + 1, line),
            )
            in_query = True
        # Existing query
        elif in_query:
            current_query.append(line)
        else:
            # probably a blank line or something
            pass
    assert (not current_query) and not in_query, "the file ended, but there was a query in progress"
    return queries


def write_random_sample(queries, format=lambda i, line: line, percentage=1.0):
    """
    Given
    - a list of `queries`,
    - a `percentage` of queries to use, and
    - a `format` function which, given the query number and query, returns the data to write for each query,
    write `percentage` queries to disk, named after their index in the list.
    Returns the number of queries written.
    """
    num_to_output = int(round(len(queries) * percentage))

    # Keep the original index of the query even though we're only taking a subset
    outputs = random.sample(list(enumerate(queries)), num_to_output)
    for query_num, query in outputs:
        # Pad filename to 4 zeros
        filename = "test{0:04d}.sql".format(query_num)
        with open(filename, 'w') as fd:
            content = format(query_num, query)
            fd.write(content)

    return num_to_output

if __name__ == '__main__':
    print("generating all queries")

    file_num = int(sys.argv[1])
    percentage = float(sys.argv[2]) if len(sys.argv) > 2 else 1

    template = """\
-- Derived from https://github.com/shivarajugowda/jdbcSQLTest/tree/master/resources/sqllogictest
--{}
{};
"""

    # Open the input relative to the python script, not the current working directory
    root = path.dirname(path.abspath(__file__))
    input_filename = path.join(root, 'select{}.test'.format(file_num))

    descriptions = []
    description = ''

    def is_start(line):
        global description
        line = line.strip()
        yes = line.startswith("query")
        if yes:
            description = line[len("query"):]
            assert description, "queries should have a description"
        return yes

    def is_end(line):
        global description
        yes = "----" in line
        if yes:
            descriptions.append(description)
            description = ''
        return yes

    with open(input_filename) as query_file:
        queries = parse_queries(query_file, is_start, is_end)

    format = lambda i, query: template.format(descriptions[i], ''.join(query))
    num_generated = write_random_sample(queries, format, percentage)
    print("all done! generated {} out of {} available queries".format(num_generated, len(queries)))
