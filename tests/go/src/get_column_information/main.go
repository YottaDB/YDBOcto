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

  rs, err := rows.ColumnTypes()
  if err != nil {
    panic(err)
  }
  for _, r := range rs {
    fmt.Printf("%v\n", r)
  }
}
