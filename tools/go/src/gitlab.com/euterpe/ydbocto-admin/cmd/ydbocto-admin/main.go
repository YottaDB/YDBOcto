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

package main

import (
	"fmt"
	"gitlab.com/euterpe/ydbocto-admin/internal/readpassword"
	"gitlab.com/euterpe/ydbocto-admin/pkg/adduser"
	"gitlab.com/euterpe/ydbocto-admin/pkg/deleteuser"
	"gitlab.com/euterpe/ydbocto-admin/pkg/showusers"
	"github.com/docopt/docopt-go"
)

func main() {
	usage := `ydbocto-admin.

Usage:
	ydbocto-admin add user <name>
	ydbocto-admin delete user <name>
	ydbocto-admin show users
`

	opts, err := docopt.ParseDoc(usage)
	if err != nil {
		fmt.Println(err)
		return
	}

	if opts["add"] == true {
		if opts["user"] == true {
		    user := opts["<name>"].(string)
		    prompt := fmt.Sprintf("Enter password for user %v: ", user)
		    // rawPassword, err := terminal.ReadPassword(int(syscall.Stdin))
		    rawPassword, err := readpassword.ReadPassword(prompt)
		    if err != nil {
			fmt.Println(err)
			return
		    }
		    _, err = adduser.AddUser(user, rawPassword)
		    if err != nil {
			fmt.Println(err)
			return
		    }
		    fmt.Printf("Successfully added user: \"%s\"\n", user)
		}
	} else if opts["delete"] == true {
		if opts["user"] == true {
		    user := opts["<name>"].(string)
		    err = deleteuser.DeleteUser(user)
		    if err != nil {
			fmt.Println(err)
			return
		    }
		    fmt.Printf("Successfully deleted user: \"%s\"\n", user)
		}
	} else if opts["show"] == true {
		if opts["users"] == true {
		    _, err = showusers.ShowUsers()
		    if err != nil {
			fmt.Println(err)
			return
		    }
		}
	}
}
