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

package ddl

const (
	Oid = iota
	Rolname = iota
	Rolsuper = iota
	Rolinherit = iota
	Rolcreaterole = iota
	Rolcreatedb = iota
	Rolcanlogin = iota
	Rolreplication = iota
	Rolbypassrls = iota
	Rolconnlimit = iota
	Rolpassword = iota
	Rolvaliduntil = iota
)

type User struct {
	oid int
	rolname string
	rolsuper int
	rolinherit int
	rolcreaterole int
	rolcreatedb int
	rolcanlogin int
	rolreplication int
	rolbypassrls int
	rolconnlimit int
	rolpassword string
	rolvaliduntil string
}
