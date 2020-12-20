//////////////////////////////////////////////////////////////////
//								//
// Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	//
// All rights reserved.						//
//								//
//	This source code contains the intellectual property	//
//	of its copyright holder(s), and is made available	//
//	under a license.  If you do not know the terms of	//
//	the license, please stop and do not read further.	//
//								//
//////////////////////////////////////////////////////////////////

package main

import (
  "database/sql"
  _ "github.com/lib/pq"
  "fmt"
  "os"
  "strings"
)

func main() {
  port := os.Args[1]
  // Build connection string using port specified by caller
  var connStr strings.Builder
  connStr.WriteString("host=127.0.0.1 port=")
  connStr.WriteString(port)
  connStr.WriteString(" user=ydb password=ydbrocks dbname=hello sslmode=disable")

  db, err := sql.Open("postgres", connStr.String())
  if err != nil {
    fmt.Printf("%v\n", err)
    panic(err)
  }

  // Verify names table has 6 rows to start with
  rows, err := db.Query("SELECT * FROM names")
  if err != nil {
    fmt.Printf("%v\n", err)
    panic(err)
  }
  print_rows(rows);

  // Insert 6 additional rows into names table (that already has 6 rows)
  idIncrement := 6	// Use variable instead of hardcoded constant in query in order to exercise extended query protocol
  rows, err = db.Query("INSERT INTO names SELECT id+$1,firstname,lastname FROM names", idIncrement)
  if err != nil {
    fmt.Printf("%v\n", err)
    panic(err)
  }
  print_rows(rows);

  // Verify names table has 12 rows
  rows, err = db.Query("SELECT * FROM names")
  if err != nil {
    fmt.Printf("%v\n", err)
    panic(err)
  }
  print_rows(rows);
}

// Helper function to print all rows returned from a query
func print_rows(rows *sql.Rows) {
  for rows.Next() {
    var id, firstName, lastName sql.NullString
    err := rows.Scan(&id, &firstName, &lastName)
    if err != nil {
      fmt.Printf("%v\n", err)
      panic(err)
    }
	if (id.Valid) {
		fmt.Printf("%v", id.String)
	}
    fmt.Printf("|")
	if (firstName.Valid) {
		fmt.Printf("%v", firstName.String)
	}
    fmt.Printf("|")
	if (lastName.Valid) {
		fmt.Printf("%v", lastName.String)
	}
    fmt.Printf("\n")
  }
  fmt.Println("---")
}

