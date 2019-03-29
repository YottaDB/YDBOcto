# YDB Octo

[![pipeline status](https://gitlab.com/YottaDB/DBMS/YDBOcto/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/DBMS/YDBOcto/commits/master)


The YottaDB Octo Database Management System is a SQL access layer built on top of the not-only-SQL database YottaDB.
It aims to provide SQL 92 compliance and exceptional performance.

Homepage https://gitlab.com/YottaDB/DBMS/YDBOcto

## Setup

Make sure YottaDB is setup and configured.
The SQL engine looks for the environment variable ydb_dist.

## Compiling -- Quickstart

Required dependencies are: build-essential cmake bison flex

YottaDB is also required, but currently not in any distribution repositories.
To download, please see the instructions at https://yottadb.com/product/get-started/

To compile Octo:

```
mkdir build
cd build
cmake ..
make
make test
```

### Optional CMake parameters

Octo uses some cmake parameters to control generation of fixed-size buffer allocations. These are:

 - STRING_BUFFER_LENGTH -- the maximum length of a string within the system; this supercedes any VARCHAR definitions
 - MAX_EXPRESSION_LENGTH -- the maximum length of an expression passed to the underlying MUMPS runtime. There is also a YDB limit on the size of these expressions
 - MAX_ROUTINE_LENGTH -- the maximum length of a generated routine. The default is 10MB
 - MEMORY_CHUNK_SIZE -- size of memory chunks to allocate; default is 32MB
 - MEMORY_CHUNK_PROTECT -- if non-zero, memory following chunks is protected to detect buffer overflows. If 2, data is placed closer to the protected region to increase the chances of detecting an error

## Usage

After building, the 'src/octo' executable provides access to the SQL interpreter.
