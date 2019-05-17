package main

import (
  "database/sql"
  _ "github.com/lib/pq"
  "fmt"
)

func main() {
  connStr := "host=127.0.0.1 port=1337 user=charles dbname=hello sslmode=disable"
  db, err := sql.Open("postgres", connStr)
  if err != nil {
    panic(err)
  }

  rows, err := db.Query("SELECT * FROM names")
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
