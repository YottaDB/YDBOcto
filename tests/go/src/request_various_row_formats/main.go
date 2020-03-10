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

// For all queries in this file, the column format type is implicitly determined by pq
// from the data type of the arguments passed to db.Query. This is done because pq doesn't provide a mechanism
// for explicitly specifying column formats.
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

  firstname := "Zero"
  lastname := "Cool"
  // All result rows are text fields, and so get 'text' result row format.
  // This tests the default case, where 0 result column formats are specified.
  rows, err := db.Query("SELECT firstname, lastname FROM names WHERE firstName = $1 OR lastname = $2", firstname, lastname)
  if err != nil {
    panic(err)
  }
  for rows.Next() {
    var firstName, lastName string
    err = rows.Scan(&firstName, &lastName)
    if err != nil {
      panic(err)
    }
    fmt.Printf("%v|%v\n", firstName, lastName)
  }
  fmt.Println("---")

  id1 := 0
  id2 := 5
  // All result columns are integer fields. Here one format is specified for all result columns, and that format is binary (1)
  rows, err = db.Query("SELECT n1.id,n2.id FROM names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE n1.id = $1 OR n2.id = $2", id1, id2)
  if err != nil {
    panic(err)
  }
  for rows.Next() {
    var id_a, id_b int
    err = rows.Scan(&id_a, &id_b)
    if err != nil {
      panic(err)
    }
    fmt.Printf("%v|%v\n", id_a, id_b)
  }
  fmt.Println("---")

  // A mixture of binary and text fields.
  // This tests the case where an exact number of column formats is specified, with a specific format for each column.
  rows, err = db.Query("SELECT n1.id,n2.id,n1.lastname,n2.firstname FROM names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE n1.id = $1 OR n2.id = $2 OR n1.lastname = $3 OR n2.firstname = $4", id1, id2, lastname, firstname)
  if err != nil {
    panic(err)
  }
  for rows.Next() {
    var id_a, id_b int
    var firstName, lastName string
    err = rows.Scan(&id_a, &id_b, &lastName, &firstName)
    if err != nil {
      panic(err)
    }
    fmt.Printf("%v|%v|%v|%v\n", id_a, id_b, lastName, firstName)
  }
  fmt.Println("---")
}
