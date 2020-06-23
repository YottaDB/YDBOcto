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
    fmt.Printf("%v\n", err)
    panic(err)
  }

  firstname := "Zero"
  rows, err := db.Query("SELECT * FROM (SELECT * FROM names) n1 WHERE firstName = $1 AND lastname = 'Cool'", firstname)
  if err != nil {
    panic(err)
  }
  for rows.Next() {
    var id, firstName, lastName sql.NullString
    err = rows.Scan(&id, &firstName, &lastName)
    if err != nil {
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
}
