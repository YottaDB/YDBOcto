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

#define TEMPLATE_SELECT_BASIC "NEW rowId,t %s SET junkVar=$I(rowId) FOR  %s SET junkVar=$I(rowId) USE:%s $P KILL:%s rowId,t Q:%s  "
#define TEMPLATE_CREATE_TABLE_START "CREATE TABLE %s (rowIdSpecialKey INTEGER PRIMARY KEY"
#define TEMPLATE_CREATE_TABLE_COLUMN ", clmn%d VARCHAR(30)"

// INPUTS: table_name
#define TEMPLATE_TABLE_DEFAULT_GLOBAL "^%s(keys(0))"
// INPUTS: table_name
#define TEMPLATE_TABLE_DEFAULT_CURSOR "SET keys(0)=$O(^%s(keys(0)))"
// INPUTS: cursor_name
#define TEMPLATE_TABLE_DEFAULT_PACK "SET storeKey=$$STOREKEY(\"\"%%s\"\",.keys),@storeKey"
// INPUTS: cursor_name
#define TEMPLATE_TABLE_DEFAULT_UNPACK "SET cursor=\"\"%%s\"\",keys(0)=$P($G(@cursor),\"\"|\"\",1)"
#define TEMPLATE_TABLE_DEFAULT_END "(\"\"\"\"=keys(0))"
#define TEMPLATE_TABLE_DEFAULT_DELIM "|"

#define TEMPLATE_INSERT_VALUES "TEMPLATE_SELECT_BASIC "

#define TEMPLATE_CREATE_XREF_TABLE "CREATE TABLE %s (tableKey VARCHAR(30) PRIMARY KEY);"
// The SELECT part gets replaced with the source; it's here as a placeholder
#define TEMPLATE_INSERT_XREF_TABLE "INSERT INTO %s (SELECT %s FROM %s);"
