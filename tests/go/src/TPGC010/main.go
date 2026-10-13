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

// YDBOcto#1158 : LIKE with a bind parameter pattern, in cases that a JDBC client cannot produce (see TPGC010.sql).
// Each query prints its rows (the ids) or its error. A query on "COALESCE(v, NULL)" is never optimized (it is not a
// column), so it checks every row: it gives the rows that the query next to it must return.
package main

import (
	"context"
	"database/sql"
	"fmt"
	"os"

	_ "github.com/lib/pq"
)

func printRows(description string, rows *sql.Rows, err error) {
	if err != nil {
		fmt.Printf("%s\n  -> ERROR: %v\n", description, err)
		return
	}
	defer rows.Close()
	ids := ""
	for rows.Next() {
		var id string
		rows.Scan(&id)
		if ids != "" {
			ids += ","
		}
		ids += id
	}
	fmt.Printf("%s\n  -> [%s]\n", description, ids)
}

func open(port string, options string) *sql.DB {
	db, err := sql.Open("postgres", "host=127.0.0.1 port="+port+" user=ydb password=ydbrocks dbname=hello sslmode=disable"+options)
	if err != nil {
		panic(err)
	}
	return db
}

func main() {
	port := os.Args[1]
	db := open(port, "")
	defer db.Close()

	// 1. $2 occurs before $1. As in PostgreSQL, $n gets the n-th value (YDBOcto#1165), so "v LIKE $2" gets 'PAT%' and
	// "v <> $1" gets '%X': the query is v LIKE 'PAT%' AND v <> '%X', and the correct rows are 1 (PAT) and 2 (PATX)
	// ("<>" compares exactly, so '%' is not a wildcard there). The LIKE at Bind must classify 'PAT%', the value that the
	// plan gets: a walk over 'PAT'. When the LIKE was classified with another value than the one the plan gets, the
	// query returned wrong rows.
	fmt.Println("# 1. $2 before $1, bound to ('%X', 'PAT%')")
	rows, err := db.Query("SELECT id FROM TPGC010 WHERE v LIKE $2 AND v <> $1 ORDER BY id", "%X", "PAT%")
	printRows("v LIKE $2 AND v <> $1", rows, err)
	rows, err = db.Query("SELECT id FROM TPGC010 WHERE COALESCE(v, NULL) LIKE $2 AND v <> $1 ORDER BY id", "%X", "PAT%")
	printRows("COALESCE(v, NULL) LIKE $2 AND v <> $1", rows, err)

	// 2. Parameter format codes of the Bind message. By default, this client sends none (all values are text).
	// With "binary_parameters=yes", it sends one format code per parameter, binary for a []byte value. A binary
	// value is not looked at for a prefix, so that query reads every row (and still returns the right rows).
	// Note: the binary values are 3 and 5 bytes long, as rocto converts a binary value of 1, 2, 4 or 8 bytes to an
	// integer whatever its type ("copy_binary_parameter()", YDBOcto#1164).
	fmt.Println("# 2. Bind with no parameter format codes, then with one per parameter (text, binary)")
	rows, err = db.Query("SELECT id FROM TPGC010 WHERE v LIKE $1 ORDER BY id", "PAT%")
	printRows("v LIKE $1 bound to 'PAT%' (no format codes)", rows, err)
	binaryDb := open(port, " binary_parameters=yes")
	defer binaryDb.Close()
	rows, err = binaryDb.Query("SELECT id FROM TPGC010 WHERE v LIKE $1 AND v <> $2 ORDER BY id", "PAT%", []byte("PAT"))
	printRows("v LIKE $1 AND v <> $2 bound to ('PAT%' text, 'PAT' binary)", rows, err)
	rows, err = binaryDb.Query("SELECT id FROM TPGC010 WHERE v LIKE $1 ORDER BY id", []byte("PATX%"))
	printRows("v LIKE $1 bound to 'PATX%' (binary)", rows, err)

	// 3. The table is dropped (by another connection) between the Parse and the Bind of a prepared statement. Bind
	// parses the query again to choose the plan for the bound value, which now fails, so Bind returns the error.
	fmt.Println("# 3. The table is dropped between Parse and Bind")
	ctx := context.Background()
	prepareConn, err := db.Conn(ctx)
	if err != nil {
		panic(err)
	}
	defer prepareConn.Close()
	dropConn, err := db.Conn(ctx)
	if err != nil {
		panic(err)
	}
	defer dropConn.Close()
	stmt, err := prepareConn.PrepareContext(ctx, "SELECT id FROM TPGC010B WHERE v LIKE $1 ORDER BY id")
	if err != nil {
		panic(err)
	}
	_, err = dropConn.ExecContext(ctx, "DROP TABLE TPGC010B")
	if err != nil {
		panic(err)
	}
	rows, err = stmt.QueryContext(ctx, "PAT%")
	printRows("v LIKE $1 bound to 'PAT%' after DROP TABLE", rows, err)
}
