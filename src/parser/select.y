sql_select_statement
  : query_specification { $$ = $1; }
  ;

sort_specification_list
  : sort_specification sort_specification_list_tail {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      SqlColumnListAlias *alias;
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      dqinit(alias);
      assert(alias->next == alias);
      // Allocate the column list
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      SqlColumnList *column_list;
      // Get the allocated column list
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      dqinit(column_list);
      // The sort spec should be a value column_REFERENCE
      SqlValue *value;
      UNPACK_SQL_STATEMENT(value, $sort_specification, value);
      assert(value->type == COLUMN_REFERENCE);
      column_list->value = $sort_specification;
    }
  ;

sort_specification_list_tail
  : /* Empty */
  | COMMA sort_specification_list
  ;

sort_specification
  : sort_key { $$ = $1; }
  | sort_key collate_clause
  | sort_key ordering_specification
  | sort_key collate_clause ordering_specification
  ;

sort_key
  : column_reference { $$ = $1; }
  | LITERAL { $$ = $1; }
  ;

ordering_specification
  : ASC
  | DESC
  ;

query_specification
  : SELECT select_list table_expression {
      $$ = $table_expression;
      assert(($$)->type == select_STATEMENT);
      ($$)->v.select->select_list = ($select_list);
      SqlJoin *join;
      UNPACK_SQL_STATEMENT(join, ($$)->v.select->table_list, join);
      SqlColumnList *column_list;
      SqlColumnListAlias *cur_column_list_alias, *start_column_list_alias;
      SqlValueType type;
      int result;
      UNPACK_SQL_STATEMENT(start_column_list_alias, $select_list, column_list_alias);
      cur_column_list_alias = start_column_list_alias;
      if(cur_column_list_alias != NULL) {
	  result = qualify_column_list_alias(cur_column_list_alias, join);
	  if(result != 0) {
	      YYABORT;
	  }
	  result = populate_data_type($select_list, &type);
	  if(result != 0) {
	      YYABORT;
	  }
      }
      ($$)->v.select->order_expression = NULL;
    }
  | SELECT set_quantifier select_list table_expression
  | SELECT select_list table_expression ORDER BY sort_specification_list {
      $$ = $table_expression;
      assert(($$)->type == select_STATEMENT);
      ($$)->v.select->select_list = ($select_list);
      SqlJoin *join;
      UNPACK_SQL_STATEMENT(join, ($$)->v.select->table_list, join);
      SqlColumnList *column_list;
      SqlColumnListAlias *cur_column_list_alias, *start_column_list_alias;
      SqlValueType type;
      int result;
      UNPACK_SQL_STATEMENT(start_column_list_alias, $select_list, column_list_alias);
      cur_column_list_alias = start_column_list_alias;
      if(cur_column_list_alias != NULL) {
	  result = qualify_column_list_alias(cur_column_list_alias, join);
	  if(result != 0) {
	      YYABORT;
	  }
	  result = populate_data_type($select_list, &type);
	  if(result != 0) {
	      YYABORT;
	  }
      }
      UNPACK_SQL_STATEMENT(start_column_list_alias, $sort_specification_list, column_list_alias);
      result = qualify_column_list_alias(start_column_list_alias, join);
      if(result != 0) {
	  YYABORT;
      }
      ($$)->v.select->order_expression = $sort_specification_list;
    }
  | SELECT set_quantifier select_list table_expression ORDER BY sort_specification_list
  ;

select_list
  : ASTERISK {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      ($$)->v.column_list = 0;
    }
  | select_sublist { $$ = $1;  }
  ;

select_sublist
  : derived_column select_sublist_tail {
      $$ = $1;
      // deviation from pattern here so we don't have to deal with "NOT_A_COLUMN" elsewhere
      if(($2) != NULL) {
        SqlColumnListAlias *list1, *list2, *t_column_list;
        UNPACK_SQL_STATEMENT(list1, $$, column_list_alias);
        UNPACK_SQL_STATEMENT(list2, $2, column_list_alias);
        dqinsert(list2, list1, t_column_list);
        //free($2);
      }
    }
  ;

select_sublist_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA select_sublist { $$ = $2; }
  ;

table_expression
  : from_clause where_clause group_by_clause having_clause {
      SQL_STATEMENT($$, select_STATEMENT);
      MALLOC_STATEMENT($$, select, SqlSelectStatement);
      ($$)->v.select->table_list = ($1);
      ($$)->v.select->where_expression = $where_clause;
      SqlJoin *join;
      UNPACK_SQL_STATEMENT(join, $from_clause, join);
      SqlValueType type;
      if(qualify_statement($where_clause, join) || populate_data_type($where_clause, &type)) {
        YYABORT;
      }
    }
  ;

set_quantifier
  : ALL
  | DISTINCT
  ;

derived_column
  : non_query_value_expression {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      SqlColumnListAlias *alias;
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      dqinit(alias);
      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      assert(($1)->type == value_STATEMENT);
      dqinit(column_list);
      column_list->value = $1;
      PACK_SQL_STATEMENT(alias->column_list, column_list, column_list);
    }
  | non_query_value_expression AS column_name {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      SqlColumnListAlias *alias;
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      dqinit(alias);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      assert(($1)->type == value_STATEMENT);
      dqinit(column_list);
      column_list->value = $1;
      PACK_SQL_STATEMENT(alias->column_list, column_list, column_list);
      alias->alias = $column_name;
    }
  ;

from_clause
  : FROM table_reference {$$ = $2; }
  ;

// Just consider these a list of values for all intensive purposes
table_reference
  : column_name table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlTable *table = find_table($column_name->v.value->v.reference);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlTableAlias *alias;
      if(table == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, $column_name->v.value->v.reference);
        print_yyloc(&($column_name)->loc);
        YYABORT;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, table_STATEMENT);
      alias->table->v.table = copy_sql_table(table);
      alias->alias = copy_sql_statement(table->tableName);
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        dqinsert(join, join_tail, t_join);
        free($table_reference_tail);
      }
    }
  | column_name correlation_specification table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlTable *table = find_table($column_name->v.value->v.reference);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlTableAlias *alias;
      if(table == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, $column_name->v.value->v.reference);
        print_yyloc(&($column_name)->loc);
        YYABORT;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, table_STATEMENT);
      alias->table->v.table = copy_sql_table(table);
      alias->alias = $correlation_specification;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        dqinsert(join, join_tail, t_join);
        free($table_reference_tail);
      }
    }
  | derived_table {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlTableAlias *alias;
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, select_STATEMENT);
      alias->table = $derived_table;
      SqlValue *value;
      char *string = "temp_table";
      int string_len = strlen(string);
      value = (SqlValue*)malloc(sizeof(SqlValue));
      value->v.string_literal = malloc(string_len + 1);
      memcpy(value->v.string_literal, string, string_len + 1);
      PACK_SQL_STATEMENT(alias->alias, value, value);
      alias->unique_id = *plan_id;
      (*plan_id)++;
      dqinit(join);
    }
  | derived_table correlation_specification {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      ($$)->v.join->type = TABLE_SPEC;
      //      ($$)->v.join->value = $1;
      dqinit(($$)->v.join);
      SqlTableAlias *alias;
      alias = (SqlTableAlias*)malloc(sizeof(SqlTableAlias));
      alias->table =  $1;
      alias->alias = $2;
      alias->unique_id = *plan_id;
      (*plan_id)++;
      //($$)->v.join->value = alias;
    }
  | joined_table { $$ = $1; }
  ;

table_reference_tail
  : /* Empty */ {
      $$ = NULL;
    }
  | COMMA table_reference { $$ = $1; }
  ;

/// TODO: what is this (column_name_list) syntax?
correlation_specification
  : optional_as column_name { $$ = $1; }
  | optional_as column_name LEFT_PAREN column_name_list RIGHT_PAREN
  ;

optional_as
  : /* Empty */
  | AS
  ;

derived_table
  : table_subquery {$$ = $1; }
  ;

joined_table
  : cross_join { $$ = $1; }
  | qualified_join
  | LEFT_PAREN joined_table RIGHT_PAREN
  ;

cross_join
  : table_reference CROSS JOIN table_reference {
      SqlJoin *left, *right, *t_join;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      left->type = CROSS_JOIN;
      dqinsert(left, right, t_join);
      free($4);
    }
  ;

qualified_join
  : table_reference JOIN table_reference join_specification
  | table_reference NATURAL JOIN table_reference join_specification
  | table_reference join_type JOIN table_reference join_specification
  | table_reference NATURAL join_type JOIN table_reference join_specification
  ;

join_specification
  : /* Empty */
  | join_condition
  | named_column_joins
  ;

named_column_joins
  : USING LEFT_PAREN join_column_list RIGHT_PAREN
  ;

join_column_list
  : column_name_list
  ;

join_condition
  : ON search_condition
  ;

join_type
  : INNER
  | outer_join_type
  | outer_join_type OUTER
//  | UNION // This conflicts with non_join_query_expression
  ;

outer_join_type
  : RIGHT
  | LEFT
  | FULL
  ;

where_clause
  : /* Empty */ { $$ = NULL; }
  | WHERE search_condition { $$ = $search_condition; }
  ;

group_by_clause
  : /* Empty */
  | GROUP BY grouping_column_reference_list
  ;

grouping_column_reference_list
  : grouping_column_reference grouping_column_reference_list_tail
  ;

grouping_column_reference_list_tail
  : /* Empty */
  | COMMA grouping_column_reference_list
  ;

grouping_column_reference
  : column_reference
  | column_reference collate_clause
  ;

having_clause
  : /* Empty */
  | HAVING search_condition
  ;
