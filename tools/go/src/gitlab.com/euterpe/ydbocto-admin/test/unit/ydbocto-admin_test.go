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
	"os"
	"log"
	"testing"
)

func TestMain(m *testing.M) {
	test_dir := "/tmp/ydbocto-admin_test"
	err := os.Mkdir(test_dir, 0700)
	if err != nil {
		log.Fatal(err)
	}
	ret_code := m.Run()
	os.RemoveAll(test_dir)
	os.Exit(ret_code)
}
