#define TEMPLATE_SELECT_BASIC "NEW rowId %s FOR  %s USE:%s $P KILL:%s rowId Q:%s  "
#define TEMPLATE_CREATE_TABLE_START "CREATE TABLE %s (rowIdSpecialKey INTEGER PRIMARY KEY"
#define TEMPLATE_CREATE_TABLE_COLUMN ", clmn%d VARCHAR(30)"

// INPUTS: table_name
#define TEMPLATE_TABLE_DEFAULT_GLOBAL "^%s(keys(0))"
// INPUTS: table_name
#define TEMPLATE_TABLE_DEFAULT_CURSOR "SET keys(0)=$O(^%s(keys(0))),j=$I(rowId)"
// INPUTS: cursor_name
#define TEMPLATE_TABLE_DEFAULT_PACK "SET storeKey=$$STOREKEY(\"\"%%s\"\",.keys),@storeKey"
// INPUTS: cursor_name
#define TEMPLATE_TABLE_DEFAULT_UNPACK "SET cursor=\"\"%%s\"\",keys(0)=$P($G(@cursor),\"\"|\"\",1)"
#define TEMPLATE_TABLE_DEFAULT_END "(\"\"\"\"=keys(0))"
#define TEMPLATE_TABLE_DEFAULT_DELIM "|"
