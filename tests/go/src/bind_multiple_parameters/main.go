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
  lastname := "Cool"
  // All literals parameterized, only outside subquery
  rows, err := db.Query("SELECT * FROM (SELECT * FROM names) n1 WHERE firstName = $1 AND lastname = $2", firstname, lastname)
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
  fmt.Println("---")

  id1 := 0
  id2 := 5
  // All literals are parameterized, one in subquery
  rows, err = db.Query("SELECT * FROM (SELECT * FROM names WHERE id = $1) n1 WHERE firstName = $2 OR id = $3", id1, firstname, id2)
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
  fmt.Println("---")

  // Half literals parameterized, half not
  rows, err = db.Query("SELECT * FROM (SELECT * FROM names WHERE id = $1) n1 WHERE firstName = $2 OR id = 5", id1, firstname)
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
  fmt.Println("---")

  // Parameterized literal comparison in WHERE issues error
  rows, err = db.Query("SELECT * FROM names WHERE $1 = $2", 0, 1)
  if err == nil {
    panic(err)
  }

  // All literals are parameterized, both within and without subquery
  rows, err = db.Query("SELECT * FROM (SELECT * FROM names WHERE id = $1 OR lastname = $2) n1 WHERE firstName = $3 OR id = $4", id1, lastname, firstname, id2)
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
  fmt.Println("---")

  // Half literals parameterized, half not, with half within a subquery and half without
  rows, err = db.Query("SELECT * FROM (SELECT * FROM names WHERE id = $1 OR lastname = 'Cool') n1 WHERE firstName = $2 OR id = 5", id1, firstname)
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
