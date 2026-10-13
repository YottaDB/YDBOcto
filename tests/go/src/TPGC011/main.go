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

// YDBOcto#1165 : Bind parameters $n are numbered as in PostgreSQL (see TPGC011.sql). Each query prints its rows (the
// ids) or its error. The rows given in the comments are those that PostgreSQL returns for the same query and values:
// the test runs this program against both rocto and PostgreSQL and compares the results.
// Arguments: port, user and database to connect to.
package main

import (
	"database/sql"
	"fmt"
	"os"

	_ "github.com/lib/pq"
)

func run(db *sql.DB, description string, query string, args ...interface{}) {
	rows, err := db.Query(query, args...)
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

func main() {
	db, err := sql.Open("postgres", "host=127.0.0.1 port="+os.Args[1]+" user="+os.Args[2]+" password=ydbrocks dbname="+os.Args[3]+" sslmode=disable")
	if err != nil {
		panic(err)
	}
	defer db.Close()

	// $2 occurs before $1: $1 is '%X' and $2 is 'PAT%', so v LIKE 'PAT%' AND v <> '%X': rows 1,2
	// (before YDBOcto#1165, LIKE got the first value and <> the second: rows 2,3,4)
	run(db, "v LIKE $2 AND v <> $1 bound to ('%X', 'PAT%')",
		"SELECT id FROM TPGC011 WHERE v LIKE $2 AND v <> $1 ORDER BY id", "%X", "PAT%")
	run(db, "COALESCE(v, NULL) LIKE $2 AND v <> $1 bound to ('%X', 'PAT%') (LIKE not optimized)",
		"SELECT id FROM TPGC011 WHERE COALESCE(v, NULL) LIKE $2 AND v <> $1 ORDER BY id", "%X", "PAT%")
	// $1 used twice, with one value: rows 1,2 (before YDBOcto#1165, the statement needed 2 values)
	run(db, "v = $1 OR v LIKE $1 || '%' bound to ('PAT')",
		"SELECT id FROM TPGC011 WHERE v = $1 OR v LIKE $1 || '%' ORDER BY id", "PAT")
	// $2 before $1 with other types: id > $2 AND v = $1, so v = 'ZX' AND id > 1: row 3
	run(db, "id > $2 AND v = $1 bound to ('ZX', 2)",
		"SELECT id FROM TPGC011 WHERE id > $2 AND v = $1 ORDER BY id", "ZX", 2)
	// $2 is used but $1 is not, and the Parse message gives no data types: PostgreSQL issues
	// "could not determine data type of parameter $1"
	run(db, "v = $2 (no $1) bound to ('ignored', 'PAT')",
		"SELECT id FROM TPGC011 WHERE v = $2 ORDER BY id", "ignored", "PAT")
	// $1 used as an INTEGER (first use) and as a VARCHAR: a type error, as in PostgreSQL
	run(db, "id = $1 OR v = $1 bound to (1)",
		"SELECT id FROM TPGC011 WHERE id = $1 OR v = $1 ORDER BY id", 1)
	// $0 does not exist: PostgreSQL issues "there is no parameter $0". (A value is passed so that the query goes
	// through Parse and Bind messages: without one, this client sends it as a simple query.)
	run(db, "v = $0 OR v = $1 bound to ('PAT')", "SELECT id FROM TPGC011 WHERE v = $0 OR v = $1 ORDER BY id", "PAT")
}
