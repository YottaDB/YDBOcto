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
	. "gitlab.com/euterpe/ydbocto-admin/internal/test_helpers"
	. "gitlab.com/euterpe/ydbocto-admin/pkg/adduser"
	. "gitlab.com/euterpe/ydbocto-admin/pkg/showusers"
)

func TestShowOneUser(t *testing.T) {
	var err error
	var totalUsers int

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	totalUsers, err = ShowUsers()
	if nil != err {
		t.Errorf("ShowUsers failed with error: %v", err)
	} else if totalUsers != 1 {
		t.Errorf("ShowUsers: totalUsers = %d, expected 1", totalUsers)
	}

	Teardown()
}

func TestShowTwoUsers(t *testing.T) {
	var err error
	var totalUsers int

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("bobby", []byte("buttons"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	totalUsers, err = ShowUsers()
	if nil != err {
		t.Errorf("ShowUsers failed with error: %v", err)
	} else if totalUsers != 2 {
		t.Errorf("ShowUsers: totalUsers = %d, expected 2", totalUsers)
	}
	Teardown()
}

func TestShowThreeUsers(t *testing.T) {
	var err error
	var totalUsers int

	Setup()
	_, err = AddUser("jon", []byte("tester"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("bobby", []byte("buttons"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	_, err = AddUser("suzy", []byte("quartz"))
	if nil != err {
		t.Errorf("AddUser failed with error: %v", err)
	}
	totalUsers, err = ShowUsers()
	if nil != err {
		t.Errorf("ShowUsers failed with error: %v", err)
	} else if totalUsers != 3 {
		t.Errorf("ShowUsers: totalUsers = %d, expected 3", totalUsers)
	}
	Teardown()
}
