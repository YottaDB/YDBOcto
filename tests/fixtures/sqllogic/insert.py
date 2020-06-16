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
with many insert statements and queries, generate a new file which has only the statements.
This also autogenerates the corresponding .zwr file.
"""

import sys
from os import path
from filter import parse_queries

print("generating all statements")

file_num = int(sys.argv[1])

# Open the input relative to the python script, not the current working directory
root = path.dirname(path.abspath(__file__))
input_filename = path.join(root, 'select{}.test'.format(file_num))

"""
Assumes lines look like this:
```
statement ok
// any non-empty line (and possibly many in a row)

```
Note the blank line at the end.
"""

is_start = lambda line: line.strip() == "statement ok"
is_end = lambda line: not line.strip()

with open(input_filename) as insert_file:
    statements = parse_queries(insert_file, is_start, is_end)

# First write the parsed SQL statements to disk
format = lambda query: ''.join(query).rstrip() + ';\n'

# {table_name: has_primary_key}
tables = {}

unique_id = 0
def add_unique_id(stmt):
    if "CREATE TABLE" in stmt[0]:
        # CREATE TABLE table_name(...)
        table_name = stmt[0].split()[2]
        paren = table_name.index('(')
        if paren != -1:
            table_name = table_name[:paren]
        assert table_name not in tables.keys(), "saw CREATE TABLE for table that has already been created"
        # Find the primary key for the table, if one exists
        primary_key = next((line for line in stmt if " PRIMARY KEY" in line), None)
        # Store this so we know whether to generate an id for INSERT statements
        tables[table_name] = primary_key is not None
        # Add our own primary key if one does not already exist.
        # This prevents catastrophically slow joins when there are many columns in a table.
        if primary_key is None:
            # CREATE TABLE table_name(id, ...)
            if len(stmt) == 1:
                assert(stmt[0].count('(') == 1), "don't know how to handle nested parentheses"
                stmt[0] = stmt[0].replace('(', "(id INTEGER PRIMARY KEY, ")
            # CREATE TABLE table_name(
            # id,
            # ...
            # )
            else:
                stmt[0] = stmt[0] + "  id INTEGER PRIMARY KEY,\n"
            primary_key = "id"
        else:
            # x INTEGER PRIMARY KEY -> x
            words = primary_key.split()
            primary_key = words[words.index("PRIMARY") - 2]
        # This is not a very smart script.
        assert stmt[-1].rstrip()[-1] == ')', \
           "Don't know how to handle CREATE TABLE statements not ending with ')' (got {})".format(stmt[-1])
        stmt[-1] = ') GLOBAL "^{}(keys(""{}""))"'.format(table_name, primary_key)
    elif "INSERT INTO" in stmt[0]:
        # INSERT INTO table_name(...)
        words = stmt[0].split()
        table_name = words[words.index("INTO") + 1]
        paren = table_name.find('(')
        if paren != -1:
            table_name = table_name[:paren].strip()
        # Add our own primary key if there wasn't one in the table already
        if not tables[table_name]:
            global unique_id
            # INSERT INTO table_name(id, ...)
            stmt[0] = stmt[0].replace("VALUES(", "VALUES({}, ".format(unique_id))
            unique_id += 1
    return stmt

with open("sqllogic{}.sql".format(file_num), 'w') as sql_file:
    sql_file.writelines(map(
        lambda line: format(add_unique_id(line)),
        statements
    ))

# Now transform the SQL syntax into something MUPIP LOAD understands
# This supports the following statements:
# - INSERT INTO table VALUES (a, b, ...)
# This will give an assertion error on the following statements:
# - INSERT INTO table (named, columns) VALUES (a, b, ...)
# All other statements are ignored.
#
# This assumes that there is no `PRIMARY KEY` column and generates its own internal unique ID to serve as a PRIMARY KEY.
# For example, `INSERT INTO t1 VALUES (1, 2, 3)` will be transformed to `^t1(1)="1|2|3"`.
# As a consequence, duplicates are always allowed and collisions cannot happen.
with open("sqllogic{}.zwr".format(file_num), 'w') as zwr_file:
    zwr_file.write("YottaDB MUPIP EXTRACT\n11-JUN-2020  13:55:45 ZWR\n")
    for stmt in statements:
        if not stmt[0].startswith("INSERT INTO "):
            continue
        assert len(stmt) == 1, "INSERT statements should only have one line"
        # Allow spaces inside the VALUES list
        table, rest = stmt[0][len("INSERT INTO "):].split(' ', 1)
        # e.g. INSERT INTO table(b, a) VALUES (b, a)
        assert '(' not in table, "named inserts are not supported (for insert statement {})".format(stmt[0])
        # NOTE: does not handle escaping at all
        values = list(map(lambda s: s.strip().replace("'", ''), rest.split(',')))
        values[0] = values[0].replace('VALUES(', '').lstrip()
        assert values[-1][-1] == ')', "INSERT INTO ... VALUES (...) should end with a ')', got {}".format(values[-1])
        # Python doesn't have a str.pop function
        values[-1] = values[-1][:-1]
        content = '^{}({})="{}"\n'.format(table, values[0], '|'.join(values[1:]))
        zwr_file.write(content)

print("finished generating statements")
