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

def line_info(line):
    start = line.strip() == "statement ok"
    end = not line.strip()
    return start, end, False

with open(input_filename) as insert_file:
    statements = parse_queries(insert_file, line_info)

# First write the parsed SQL statements to disk
format = lambda query: ''.join(query).rstrip() + ';\n'

# {table_name: (has_primary_key, [column, ...])}
tables = {}

unique_id = 0
def add_unique_id(stmt):
    if "CREATE TABLE" in stmt[0]:
        # CREATE TABLE table_name(...)
        table_name = stmt[0].split()[2]
        table_name = table_name[:table_name.index('(')]
        assert table_name not in tables.keys(), "saw CREATE TABLE for table that has already been created"
        # Find the primary key for the table, if one exists
        primary_key = next((line for line in stmt if " PRIMARY KEY" in line), None)
        has_primary_key = primary_key is not None
        # Find the column names so we can support named columns in INSERTs
        if stmt[0].find(')') == -1:
            print("warning: currently named columns for `CREATE TABLE` statements must all be on one line")
            print("warning: continuing, but named inserts will not work") # not work -> assertion failure, not data corruption
            columns = []
        else:
            # stmt[0]: "... (a INTEGER, b INTEGER PRIMARY KEY, c VARCHAR)"
            # [a INTEGER, ...]
            create = stmt[0]
            assert create.count(')') == 1, "`VARCHAR(30)` or any other types with nested parentheses are not supported"
            columns = create[create.index('(') + 1:create.index(')')].split(',')
            # Keep only the names: [a, b, c]
            columns = [column.strip().split(' ')[0] for column in columns]
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
            # Our generated ID will be the first column.
            primary_key = "id"
            if columns:
                columns = [primary_key] + columns
        else:
            # x INTEGER PRIMARY KEY -> x
            words = primary_key.split()
            primary_key = words[words.index("PRIMARY") - 2]
        print(columns)

        # Store the columns so we know whether to generate an id for INSERT statements and how to handle named columns
        tables[table_name] = (has_primary_key, columns)

        # This is not a very smart script.
        assert stmt[-1].rstrip()[-1] == ')', \
           "Don't know how to handle CREATE TABLE statements not ending with ')' (got {})".format(stmt[-1])
        stmt[-1] += ' GLOBAL "^{}(keys(""{}""))"'.format(table_name, primary_key)
    elif "INSERT INTO" in stmt[0]:
        # INSERT INTO table_name(...)
        words = stmt[0].split()
        table_name = words[words.index("INTO") + 1]
        paren = table_name.find('(')
        if paren != -1:
            table_name = table_name[:paren].strip()
        # Add our own primary key if there wasn't one in the table already
        if not tables[table_name][0]:
            global unique_id
            # INSERT INTO table_name(id, ...)
            stmt[0] = stmt[0].replace(table_name + '(', table_name + '(id,')
            stmt[0] = stmt[0].replace("VALUES(", "VALUES({},".format(unique_id))
            print("inserting primary key")
            print(stmt[0])
            unique_id += 1
    return stmt

with open("sqllogic{}.sql".format(file_num), 'w') as sql_file:
    sql_file.writelines(format(add_unique_id(line)) for line in statements)

# Now transform the SQL syntax into something MUPIP LOAD understands
# This supports the following statements:
# - INSERT INTO table VALUES (a, b, ...)
# - INSERT INTO table (named, columns) VALUES (b, a, ...)
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
        # NOTE: does not handle escaping at all
        values = [s.strip().replace("'", '') for s in rest.split(',')]
        values[0] = values[0].split('VALUES(', 1)[1].lstrip()
        assert values[-1][-1] == ')', "INSERT INTO ... VALUES (...) should end with a ')', got {}".format(values[-1])
        # Python doesn't have a str.pop function
        values[-1] = values[-1][:-1]

        # named inserts, e.g. INSERT INTO table(b, a) VALUES (b, a)
        if '(' in table:
            start, end = table.index('('), table.index(')')
            insert_columns = table[start + 1:end].replace(' ', '').split(',')
            table = table[:start]
            schema_columns = tables[table][1]
            assert schema_columns, "attempted a named insert, but the column names for table {} are unknown".format(table)
            # column = (idx, name)
            # Sort the inserts by the index of the column in the schema.
            # https://stackoverflow.com/a/6422808/7669110
            # For example, given
            # - schema_columns = (a, b, c)
            # - insert_columns = (b, c, a)
            # - values = (1, 2, 3)
            # outputs (3, 1, 2)
            values = [column[1] for column in sorted(enumerate(values), key=lambda column: schema_columns.index(insert_columns[column[0]]))]
            print(values)
            #assert False, "named inserts are not supported (for insert statement {})".format(stmt[0])

        # Now replace NULL with the empty string
        def replace_null(val):
            if val == 'NULL':
                return ''
            else:
                return val
        values = list(map(replace_null, values))

        # Finally write out to disk.
        content = '^{}({})="{}"\n'.format(table, values[0], '|'.join(values[1:]))
        zwr_file.write(content)

print("finished generating statements")
