/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

package showusers

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"lang.yottadb.com/go/yottadb"
	. "gitlab.com/euterpe/ydbocto-admin/internal/ddl"
)

// ShowUsers retrieves all user entries from the database, prints them to stdout,
// and returns the total number retrieved for convenience.
// Assumes existence of the relevant global variable.
func ShowUsers() (int, error) {
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	var subserr error
	user := ""

	varname := "^%ydboctoocto"
	users := make(map[int]string)
	for subserr == nil {
		user, subserr = yottadb.SubNextE(tptoken, &errstr, varname, []string{"users", user})
		if user == "" {
			break
		}
		row, err := yottadb.ValE(tptoken, &errstr, varname, []string{"users", user})
		if nil != err {
			return 0, err
		}
		columns := strings.Split(row, "|")
		userId := columns[Oid]
		i, err := strconv.ParseInt(userId, 10, 0)
		users[int(i)] = user
	}

	totalUsers := len(users)
	if  totalUsers <= 0 {
		fmt.Println("No YDBOcto users found.")
	} else {
		fmt.Println("Current YDBOcto users, by ID:")
		var keys []int
		for k := range users {
			keys = append(keys, k)
		}
		sort.Ints(keys)
		for _, i := range keys {
			fmt.Printf("%-8d%s\n", i, users[i])
		}
	}
    return totalUsers, nil
}
