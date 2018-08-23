# YDB DBMS

[![pipeline status](https://gitlab.com/YottaDB/Octo/YDBDBMS/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/Octo/YDBDBMS/commits/master)


The YottaDB Database Management System is a SQL access layer built on top of the not-only-SQL database YottaDB.
It aims to provide SQL 92 compliance and exceptional performance.

Homepage https://gitlab.com/YottaDB/Octo/YDBDBMS

## Setup

Make sure YottaDB is setup and configured.
The SQL engine looks for the environment variable ydb_dist.

## Compiling -- Quickstart

To compile:

```
mkdir build
cd build
cmake ../src
make
make test
```

## Usage

After building, the 'octo' executable provides access to the SQL interpreter.
