//////////////////////////////////////////////////////////////////
//								//
// Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	//
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
    panic(err)
  }

  fmt.Printf("--- SELECT * FROM names; ---\n")
  // Check column types from `names` table (INT and VARCHAR fields)
  rows, err := db.Query("SELECT * FROM names")
  if err != nil {
    panic(err)
  }
  rs, err := rows.ColumnTypes()
  if err != nil {
    panic(err)
  }
  for _, r := range rs {
    fmt.Printf("%v\n", r)
  }

  // Check column types from `Employees` table from Northwind database (INT, VARCHAR, and DATE fields)
  fmt.Printf("\n--- SELECT * FROM Employees; ---\n")
  rows, err = db.Query("SELECT * FROM Employees")
  if err != nil {
    panic(err)
  }
  rs, err = rows.ColumnTypes()
  if err != nil {
    panic(err)
  }
  for _, r := range rs {
    fmt.Printf("%v\n", r)
  }

  // Check column types from `Products` table from Northwind database (INT, VARCHAR, and NUMERIC fields)
  fmt.Printf("\n--- SELECT * FROM Products; ---\n")
  rows, err = db.Query("SELECT * FROM Products")
  if err != nil {
    panic(err)
  }
  rs, err = rows.ColumnTypes()
  if err != nil {
    panic(err)
  }
  for _, r := range rs {
    fmt.Printf("%v\n", r)
  }

  // Check column types from `pg_type` table from PostgreSQL catalog database (INT, VARCHAR, and BOOLEAN fields)
  fmt.Printf("\n--- SELECT * FROM pg_catalog.pg_type; ---\n")
  rows, err = db.Query("SELECT * FROM pg_catalog.pg_type")
  if err != nil {
    panic(err)
  }
  rs, err = rows.ColumnTypes()
  if err != nil {
    panic(err)
  }
  for _, r := range rs {
    fmt.Printf("%v\n", r)
  }
}
