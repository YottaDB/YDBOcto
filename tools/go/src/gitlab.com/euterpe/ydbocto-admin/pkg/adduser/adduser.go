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

package adduser

import (
	"errors"
	"strings"
	"crypto/md5"
	"encoding/hex"
	"lang.yottadb.com/go/yottadb"
	. "gitlab.com/euterpe/ydbocto-admin/internal/ddl"
)

// HashMd5Password hashes a password using a username as a salt to produce
// an md5 password string in accordance with the PostgreSQL spec.
func HashMd5Password(username string, rawPassword []byte) string {
	var password strings.Builder
	password.Write(rawPassword)
	password.WriteString(username)
	hashedPassword := md5.Sum([]byte(password.String()))

	var md5Password strings.Builder
	md5Password.WriteString("md5")
	md5Password.WriteString(hex.EncodeToString(hashedPassword[:]))
	return md5Password.String()
}

// AddUser creates a new database user, hashes the user's password, and stores it in the database.
// Assumes existence of the relevant global variable.
func AddUser(username string, rawPassword []byte) (newId string, err error) {
	if username == "" {
		err = errors.New("AddUser: user name cannot be empty string")
		return "", err
	}
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	varname := "^%ydboctoocto"

	newId, err = yottadb.IncrE(tptoken, &errstr, "", varname, []string{"users"})
	if nil != err {
		return "", err
	}

	var ddl_string strings.Builder
	ddl_string.WriteString(newId)
	ddl_string.WriteString("|")
	ddl_string.WriteString(username)
	// Delimit empty columns
	for i := Rolname; i < Rolpassword; i++ {
		ddl_string.WriteString("|")
	}
	md5Password := HashMd5Password(username, rawPassword)
	ddl_string.WriteString(md5Password)
	ddl_string.WriteString("|")

	err = yottadb.SetValE(tptoken, &errstr, ddl_string.String(), varname, []string{"users", username})
	if nil != err {
		return "", err
	}

	/*
	err = yottadb.SetValE(tptoken, &errstr, newId, varname, []string{"users", username, "id"})
	if nil != err {
		return "", err
	}

	err = yottadb.SetValE(tptoken, &errstr, username, varname, []string{"users", username, "rolname"})
	if nil != err {
		return "", err
	}

	md5Password := HashMd5Password(username, rawPassword)
	err = yottadb.SetValE(tptoken, &errstr, md5Password, varname, []string{"users", username, "rolpassword"})
	if nil != err {
		return "", err
	}
	*/
	return newId, nil
}
