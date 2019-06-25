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

package deleteuser

import (
	"errors"
	"lang.yottadb.com/go/yottadb"
)

// DeleteUser creates a new database user, hashes the user's password, and stores it in the database.
// Assumes existence of the relevant global variable.
func DeleteUser(username string) (err error) {
	if username == "" {
		err = errors.New("DeleteUser: user name cannot be empty string")
		return err
	}
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	varname := "^%ydboctoocto"

	err = yottadb.DeleteE(tptoken, &errstr, yottadb.YDB_DEL_TREE, varname, []string{"users", username})
	if nil != err {
		return err
	}
	return nil
}
