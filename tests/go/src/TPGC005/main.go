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

  // Build up long query string to keep line length under 132
  var queryString strings.Builder
  queryString.WriteString("select * from names where id = $1 OR firstname = $2 OR id = $3 OR firstname = $4 OR")
  // Include some literals to bump parse_context->total_parms > parse_context->num_bind_parms
  queryString.WriteString(" id = 1 OR firstname = 'Zero' OR id = 2 OR firstname = 'Acid' OR")
  queryString.WriteString(" id = 1 OR firstname = 'Zero' OR id = 2 OR firstname = 'Acid' OR")
  queryString.WriteString(" id = 1 OR firstname = 'Zero' OR id = 2 OR firstname = 'Acid' OR")
  queryString.WriteString(" id = 1 OR firstname = 'Zero' OR id = 2 OR firstname = 'Acid' OR")
  queryString.WriteString(" id = $5 OR firstname = $6 OR id = $7 OR firstname = $8 OR")
  queryString.WriteString(" id = $9 OR firstname = $10 OR id = $11 OR firstname = $12 OR")
  queryString.WriteString(" id = $13 OR firstname = $14 OR id = $15 OR firstname = $16 OR")
  queryString.WriteString(" id = $17 OR firstname = $18 OR id = $19 OR firstname = $20")

  // Send a bunch of parameters to test parse_context->is_bind_parm array expansion
  rows, err := db.Query(queryString.String(), 0, "Zero", 1, "Lord", 2, "Acid", 3, "Joey", 4, "Cereal", 5, "Zero", 6, "Lord", 0, "Acid", 1,
                "Joey", 2, "Cereal")
  if err != nil {
    panic(err)
  }
  for rows.Next() {
    var id, firstName, lastName string
    err = rows.Scan(&id, &firstName, &lastName)
    if err != nil {
      panic(err)
    }
    fmt.Printf("%v|%v|%v\n", id, firstName, lastName)
  }
}
