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

// YDBOcto#1020 : a range whose bounds are bind parameters ($1, $2), sent with Parse and Bind messages. Each query prints
// the ids of its rows. The test runs this program against both rocto and PostgreSQL and compares the results. The table
// is "names" (ids 0 to 5: Zero, Acid, Cereal, Lord, Joey, Zero).
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

	// The key column: 1,2,3
	run(db, "id BETWEEN $1 AND $2 bound to (1, 3)", "SELECT id FROM names WHERE id BETWEEN $1 AND $2 ORDER BY id", 1, 3)
	// The same statement with other values, which runs the same plan: 3,4,5
	run(db, "id BETWEEN $1 AND $2 bound to (3, 7)", "SELECT id FROM names WHERE id BETWEEN $1 AND $2 ORDER BY id", 3, 7)
	// An empty range: no row
	run(db, "id BETWEEN $1 AND $2 bound to (3, 1)", "SELECT id FROM names WHERE id BETWEEN $1 AND $2 ORDER BY id", 3, 1)
	// Two conditions, the upper bound first: 2,3,4
	run(db, "id < $1 AND id >= $2 bound to (5, 2)", "SELECT id FROM names WHERE id < $1 AND id >= $2 ORDER BY id", 5, 2)
	// In descending order, with a LIMIT: 4,3
	run(db, "id BETWEEN $1 AND $2 ORDER BY id DESC LIMIT 2 bound to (1, 4)",
		"SELECT id FROM names WHERE id BETWEEN $1 AND $2 ORDER BY id DESC LIMIT 2", 1, 4)
	// A column that is not a key column (its AIM cross reference): Acid 1, Cereal 2, Joey 4 and Lord 3
	run(db, "firstname BETWEEN $1 AND $2 bound to ('A', 'M')",
		"SELECT id FROM names WHERE firstname BETWEEN $1 AND $2 ORDER BY firstname, id", "A", "M")
	// One bound a parameter, one a literal: 4,5
	run(db, "id > $1 AND id <= 5 bound to (3)", "SELECT id FROM names WHERE id > $1 AND id <= 5 ORDER BY id", 3)
}
