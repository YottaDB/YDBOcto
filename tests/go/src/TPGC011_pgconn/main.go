//////////////////////////////////////////////////////////////////
//								//
// Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	//
// All rights reserved.						//
//								//
//	This source code contains the intellectual property	//
//	of its copyright holder(s), and is made available	//
//	under a license.  If you do not know the terms of	//
//	the license, please stop and do not read further.	//
//								//
//////////////////////////////////////////////////////////////////

// YDBOcto#1165 : A bind parameter that the query does not use (a gap, e.g. $1 when only $2 is used) must have its data
// type in the Parse message, as in PostgreSQL. This uses pgconn (the low-level part of pgx) instead of lib/pq, because
// lib/pq cannot send data types in the Parse message. Each statement prints the parameter data types that rocto
// describes (OIDs; 1043 is VARCHAR) and its rows (the ids), or its error. The results given in the comments are those
// that PostgreSQL returns for the same statement and values: the test runs this program against both rocto and
// PostgreSQL and compares the results. Arguments: port, user and database to connect to.
package main

import (
	"context"
	"fmt"
	"os"

	"github.com/jackc/pgx/v5/pgconn"
)

func run(conn *pgconn.PgConn, name string, description string, query string, types []uint32, args ...string) {
	ctx := context.Background()
	fmt.Printf("%s\n", description)
	statement, err := conn.Prepare(ctx, name, query, types)
	if err != nil {
		fmt.Printf("  -> Parse ERROR: %v\n", err)
		return
	}
	fmt.Printf("  -> parameter types %v\n", statement.ParamOIDs)
	values := make([][]byte, len(args))
	for i, arg := range args {
		values[i] = []byte(arg)
	}
	result := conn.ExecPrepared(ctx, name, values, nil, nil).Read()
	if result.Err != nil {
		fmt.Printf("  -> ERROR: %v\n", result.Err)
		return
	}
	ids := ""
	for _, row := range result.Rows {
		if ids != "" {
			ids += ","
		}
		ids += string(row[0])
	}
	fmt.Printf("  -> [%s]\n", ids)
}

func main() {
	ctx := context.Background()
	conn, err := pgconn.Connect(ctx, "host=127.0.0.1 port="+os.Args[1]+" user="+os.Args[2]+" password=ydbrocks dbname="+os.Args[3]+" sslmode=disable")
	if err != nil {
		panic(err)
	}
	defer conn.Close(ctx)

	// $1 is not used, and the Parse message gives it VARCHAR: v = 'PAT', row 1. The value of $1 is ignored.
	run(conn, "q1", "v = $2 (no $1), Parse types (VARCHAR, VARCHAR), bound to ('ignored', 'PAT')",
		"SELECT id FROM TPGC011 WHERE v = $2 ORDER BY id", []uint32{1043, 1043}, "ignored", "PAT")
	// The Parse message gives a type for $1 only: $2 takes its type from its use. Row 1.
	run(conn, "q2", "v = $2 (no $1), Parse types (VARCHAR), bound to ('ignored', 'PAT')",
		"SELECT id FROM TPGC011 WHERE v = $2 ORDER BY id", []uint32{1043}, "ignored", "PAT")
	// Type 0 (unspecified) for $1 is the same as no type: PostgreSQL issues
	// "could not determine data type of parameter $1" (42P18) on the Parse message
	run(conn, "q3", "v = $2 (no $1), Parse types (unspecified, VARCHAR)",
		"SELECT id FROM TPGC011 WHERE v = $2 ORDER BY id", []uint32{0, 1043}, "ignored", "PAT")
	// No types at all: the same error
	run(conn, "q4", "v = $2 (no $1), no Parse types",
		"SELECT id FROM TPGC011 WHERE v = $2 ORDER BY id", nil, "ignored", "PAT")
}
