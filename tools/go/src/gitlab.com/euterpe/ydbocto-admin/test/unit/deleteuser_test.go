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

package admin_test

import (
	"testing"
	"lang.yottadb.com/go/yottadb"
	. "gitlab.com/euterpe/ydbocto-admin/internal/test_helpers"
	. "gitlab.com/euterpe/ydbocto-admin/pkg/adduser"
	. "gitlab.com/euterpe/ydbocto-admin/pkg/deleteuser"
)

func TestDeleteUserEmptyString(t *testing.T) {
	var err error

	Setup()
	err = DeleteUser("")
	if nil == err {
		t.Errorf("DeleteUser: no error where expected")
	}
	Teardown()
}

func TestDeleteOneUser(t *testing.T) {
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	var err error
	var result uint32

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}

	err = DeleteUser("jon")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}

	varname := "^%ydboctoocto"
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "jon"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "jon")
		}
	}
	Teardown()
}

func TestDeleteTwoUsers(t *testing.T) {
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	var err error
	var result uint32

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("acteon", []byte("cereal"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}

	err = DeleteUser("jon")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}
	err = DeleteUser("acteon")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}

	varname := "^%ydboctoocto"
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "jon"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "jon")
		}
	}
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "acteon"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "acteon")
		}
	}
	Teardown()
}
func TestDeleteThreeUsers(t *testing.T) {
	var tptoken uint64 = yottadb.NOTTP
	var errstr yottadb.BufferT
	var err error
	var result uint32

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("acteon", []byte("cereal"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("joe", []byte("bajugas"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}

	err = DeleteUser("jon")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}
	err = DeleteUser("acteon")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}
	err = DeleteUser("joe")
	if nil != err {
		t.Errorf("DeleteUser failed with error: %v", err)
	}

	varname := "^%ydboctoocto"
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "jon"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "jon")
		}
	}
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "acteon"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "acteon")
		}
	}
	result, err = yottadb.DataE(tptoken, &errstr, varname, []string{"users", "joe"})
	if nil != err {
		t.Errorf("YDBGo failed with error: %v", err)
	} else {
		if 0 != result {
			t.Errorf("DeleteUser failed to delete user: %v", "joe")
		}
	}
	Teardown()
}
