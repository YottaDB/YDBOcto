sql_select_statement
  : query_specification optional_query_words {
      $$ = $1;
      SqlOptionalKeyword *select_words, *new_words, *t;
      UNPACK_SQL_STATEMENT(select_words, ($$)->v.select->optional_words, keyword);
      UNPACK_SQL_STATEMENT(new_words, $optional_query_words, keyword);
      dqinsert(select_words, new_words, t);
    }
  ;


optional_query_words
  : optional_query_word_element optional_query_word_tail {
      $$ = $optional_query_word_element;
      SqlOptionalKeyword *keyword, *t_keyword;
      UNPACK_SQL_STATEMENT(keyword, $optional_query_word_tail, keyword);
      dqinsert(keyword, ($$)->v.keyword, t_keyword);
    }
  | /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  ;

 optional_query_word_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | COMMA optional_query_words { $$ = $1; }
  ;

optional_query_word_element
  : LIMIT literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_LIMIT;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
  }
/*  | UNION ALL sql_select_statement {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_UNION_ALL;
      ($$)->v.keyword->v = $3;
      dqinit(($$)->v.keyword);
  }
  | UNION sql_select_statement {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_UNION;
      ($$)->v.keyword->v = $2;
      dqinit(($$)->v.keyword);
      }*/
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
  : SELECT set_quantifier select_list table_expression {
      $$ = $table_expression;
      assert(($$)->type == select_STATEMENT);
      SqlSelectStatement *select;
      UNPACK_SQL_STATEMENT(select, $$, select);
      select->select_list = ($select_list);
      // If the select list is empty, we need all columns from the joins in
      //  the order in which they are mentioned
      SqlJoin *join, *cur_join, *start_join;
      SqlTableAlias *table_alias;
      SqlStatement *t_stmt;
      SqlColumn *t_column;
      SqlColumnListAlias *cl_alias = NULL, *t_cl_alias, *tt_cl_alias;
      UNPACK_SQL_STATEMENT(join, select->table_list, join);
      if(select->select_list->v.column_list_alias == NULL) {
          start_join = cur_join = join;
          do {
              UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
              t_column = column_list_alias_to_columns(table_alias);
              t_cl_alias = lp_columns_to_column_list(t_column, table_alias);
              if(cl_alias == NULL) {
                  cl_alias = t_cl_alias;
              } else {
                  dqinsert(cl_alias, t_cl_alias, tt_cl_alias);
              }
              cur_join = cur_join->next;
          } while(cur_join != start_join);
          select->select_list->v.column_list_alias = cl_alias;
      }
      ($$)->v.select->optional_words = $set_quantifier;
      SqlColumnList *column_list;
      SqlColumnListAlias *cur_column_list_alias, *start_column_list_alias;
      SqlValueType type;
      int result;
      UNPACK_SQL_STATEMENT(start_column_list_alias, $select_list, column_list_alias);
      cur_column_list_alias = start_column_list_alias;
      if(cur_column_list_alias != NULL) {
          result = qualify_column_list_alias(cur_column_list_alias, join);
          if(result != 0) {
              YYERROR;
          }
          result = populate_data_type($select_list, &type);
          if(result != 0) {
              YYERROR;
          }
      }
      ($$)->v.select->order_expression = NULL;
      result = qualify_join_conditions(join, join);
      if(result != 0) {
          YYERROR;
      }
    }
  | SELECT set_quantifier select_list table_expression ORDER BY sort_specification_list {
      $$ = $table_expression;
      assert(($$)->type == select_STATEMENT);
            SqlSelectStatement *select;
      UNPACK_SQL_STATEMENT(select, $$, select);
      select->select_list = ($select_list);
      // If the select list is empty, we need all columns from the joins in
      //  the order in which they are mentioned
      SqlJoin *join, *cur_join, *start_join;
      SqlTableAlias *table_alias;
      SqlStatement *t_stmt;
      SqlColumn *t_column;
      SqlColumnListAlias *cl_alias = NULL, *t_cl_alias, *tt_cl_alias;
      UNPACK_SQL_STATEMENT(join, select->table_list, join);
      if(select->select_list->v.column_list_alias == NULL) {
          start_join = cur_join = join;
          do {
              UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
              t_column = column_list_alias_to_columns(table_alias);
              t_cl_alias = lp_columns_to_column_list(t_column, table_alias);
              if(cl_alias == NULL) {
                  cl_alias = t_cl_alias;
              } else {
                  dqinsert(cl_alias, t_cl_alias, tt_cl_alias);
              }
              cur_join = cur_join->next;
          } while(cur_join != start_join);
          PACK_SQL_STATEMENT(select->select_list, cl_alias, column_list_alias);
      }
      ($$)->v.select->optional_words = $set_quantifier;
      SqlColumnList *column_list;
      SqlColumnListAlias *cur_column_list_alias, *start_column_list_alias;
      SqlValueType type;
      int result;
      UNPACK_SQL_STATEMENT(start_column_list_alias, $select_list, column_list_alias);
      cur_column_list_alias = start_column_list_alias;
      if(cur_column_list_alias != NULL) {
          result = qualify_column_list_alias(cur_column_list_alias, join);
          if(result != 0) {
              YYERROR;
          }
          result = populate_data_type($select_list, &type);
          if(result != 0) {
              YYERROR;
          }
      }
      UNPACK_SQL_STATEMENT(start_column_list_alias, $sort_specification_list, column_list_alias);
      result = qualify_column_list_alias(start_column_list_alias, join);
      if(result != 0) {
          YYERROR;
      }
      ($$)->v.select->order_expression = $sort_specification_list;
      result = qualify_join_conditions(join, join);
      if(result != 0) {
          YYERROR;
      }
    }
  | SELECT set_quantifier select_list {
      // We're going to run against a secret table with one row so the list gets found
      SqlJoin *join, *cur_join, *start_join;
      SqlTable *table;
      SqlStatement *join_statement;
      SqlTableAlias *table_alias, *alias;

      SQL_STATEMENT($$, select_STATEMENT);
      MALLOC_STATEMENT($$, select, SqlSelectStatement);
      SQL_STATEMENT(join_statement, join_STATEMENT);
      MALLOC_STATEMENT(join_statement, join, SqlJoin);
      UNPACK_SQL_STATEMENT(join, join_statement, join);
      dqinit(join);
      table = find_table("octoOneRowTable");
      if(table == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, "octoOneRowTable");
        print_yyloc(&($select_list)->loc);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, table_STATEMENT);
      alias->table->v.table = table;
      alias->alias = table->tableName;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      ($$)->v.select->table_list = join_statement;

      assert(($$)->type == select_STATEMENT);
      SqlSelectStatement *select;
      UNPACK_SQL_STATEMENT(select, $$, select);
      select->select_list = ($select_list);
      // If the select list is empty, we need all columns from the joins in
      //  the order in which they are mentioned
      SqlStatement *t_stmt;
      SqlColumn *t_column;
      SqlColumnListAlias *cl_alias = NULL, *t_cl_alias, *tt_cl_alias;
      UNPACK_SQL_STATEMENT(join, select->table_list, join);
      if(select->select_list->v.column_list_alias == NULL) {
          start_join = cur_join = join;
          do {
              UNPACK_SQL_STATEMENT(table_alias, cur_join->value, table_alias);
              t_column = column_list_alias_to_columns(table_alias);
              t_cl_alias = lp_columns_to_column_list(t_column, table_alias);
              if(cl_alias == NULL) {
                  cl_alias = t_cl_alias;
              } else {
                  dqinsert(cl_alias, t_cl_alias, tt_cl_alias);
              }
              cur_join = cur_join->next;
          } while(cur_join != start_join);
          select->select_list->v.column_list_alias = cl_alias;
      }
      ($$)->v.select->optional_words = $set_quantifier;
      SqlColumnList *column_list;
      SqlColumnListAlias *cur_column_list_alias, *start_column_list_alias;
      SqlValueType type;
      int result;
      UNPACK_SQL_STATEMENT(start_column_list_alias, $select_list, column_list_alias);
      cur_column_list_alias = start_column_list_alias;
      if(cur_column_list_alias != NULL) {
          result = qualify_column_list_alias(cur_column_list_alias, join);
          if(result != 0) {
              YYERROR;
          }
          result = populate_data_type($select_list, &type);
          if(result != 0) {
              YYERROR;
          }
      }
      ($$)->v.select->order_expression = NULL;
      result = qualify_join_conditions(join, join);
      if(result != 0) {
          YYERROR;
      }
    }
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
        YYERROR;
      }
    }
  ;

set_quantifier
  : /* Empty; default ALL */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | ALL {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | DISTINCT {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_DISTINCT;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
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
      /// TODO: we should search here for a reasonable "name" for the column
      SQL_STATEMENT(alias->alias, value_STATEMENT);
      MALLOC_STATEMENT(alias->alias, value, SqlValue);
      alias->alias->v.value->type = STRING_LITERAL;
      alias->alias->v.value->v.string_literal = malloc(strlen(" ") + 2);
      strcpy(alias->alias->v.value->v.string_literal, " ");
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
      SqlColumn *column;
      SqlTableAlias *alias;
      if(table == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, $column_name->v.value->v.reference);
        print_yyloc(&($column_name)->loc);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, table_STATEMENT);
      alias->table->v.table = table;
      alias->alias = table->tableName;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      UNPACK_SQL_STATEMENT(column, table->columns, column);
      PACK_SQL_STATEMENT(alias->column_list,
                         lp_columns_to_column_list(column, alias), column_list_alias);
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        join->type = CROSS_JOIN;
        dqinsert(join, join_tail, t_join);
      }
    }
  | column_name correlation_specification table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlTable *table = find_table($column_name->v.value->v.reference);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlColumn *column;
      SqlTableAlias *alias;
      if(table == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, $column_name->v.value->v.reference);
        print_yyloc(&($column_name)->loc);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, table_STATEMENT);
      alias->table->v.table = table;
      alias->alias = $correlation_specification;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      UNPACK_SQL_STATEMENT(column, table->columns, column);
      PACK_SQL_STATEMENT(alias->column_list,
                         lp_columns_to_column_list(column, alias), column_list_alias);
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        join->type = CROSS_JOIN;
        dqinsert(join, join_tail, t_join);
      }
    }
  | derived_table {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlTableAlias *alias;
      SqlSelectStatement *select;
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
      UNPACK_SQL_STATEMENT(select, $1, select);
      alias->column_list = select->select_list;
      /*      // Take ownership of each of the columns in the column list
      SqlColumnListAlias *cl_cur, *cl_start;
      UNPACK_SQL_STATEMENT(cl_start, alias->column_list, column_list_alias);
      cl_cur = cl_start;
      do {
      } while (cl_cur != cl_start);*/
      dqinit(join);
    }
  | derived_table correlation_specification {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlJoin *join = $$->v.join, *join_tail, *t_join;
      SqlTableAlias *alias;
      SqlSelectStatement *select;
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT(alias->table, select_STATEMENT);
      alias->table = $derived_table;
      SqlValue *value;
      alias->alias = $correlation_specification;
      alias->unique_id = *plan_id;
      (*plan_id)++;
      UNPACK_SQL_STATEMENT(select, $1, select);
      alias->column_list = select->select_list;
      dqinit(join);
    }
  | joined_table { $$ = $1; }
  ;

table_reference_tail
  : /* Empty */ {
      $$ = NULL;
    }
  | COMMA table_reference { $$ = $table_reference; }
  ;

/// TODO: what is this (column_name_list) syntax?
correlation_specification
  : optional_as column_name { $$ = $column_name; }
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
  | qualified_join { $$ = $1; }
  | LEFT_PAREN joined_table RIGHT_PAREN { $$ = $2; }
  ;

cross_join
  : table_reference CROSS JOIN table_reference {
      SqlJoin *left, *right, *t_join;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      left->type = CROSS_JOIN;
      dqinsert(left, right, t_join);
    }
  ;

qualified_join
  : table_reference JOIN table_reference join_specification {
      SqlJoin *left, *right, *t_join;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $3, join);
      right->type = INNER_JOIN;
      right->condition = $join_specification;
      dqinsert(left, right, t_join);
    }
  | table_reference NATURAL JOIN table_reference optional_join_specification {
      SqlJoin *left, *right, *t_join;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      left->type = NATURAL_JOIN;
      assert(left->condition == NULL);
      if($optional_join_specification == NULL) {
        left->condition = natural_join_condition($1, $4);
      } else {
        left->condition = $optional_join_specification;
      }
      dqinsert(left, right, t_join);
    }

  | table_reference join_type JOIN table_reference join_specification {
      SqlJoin *left, *right, *t_join;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      UNPACK_SQL_STATEMENT(right->type, $join_type, join_type);
      right->condition = $join_specification;
      dqinsert(left, right, t_join);
    }
  | table_reference NATURAL join_type JOIN table_reference join_specification
  ;

optional_join_specification
  : /* Empty */ { $$ = NULL; }
  | join_specification { $$ = $1; }
  ;

join_specification
  : join_condition { $$ = $1; }
  | named_column_joins
  ;

named_column_joins
  : USING LEFT_PAREN join_column_list RIGHT_PAREN
  ;

join_column_list
  : column_name_list
  ;

join_condition
  : ON search_condition { $$ = $2; }
  ;

join_type
  : INNER { SQL_STATEMENT($$, join_type_STATEMENT); ($$)->v.join_type = INNER_JOIN; }
  | outer_join_type { $$ = $1; }
// AFAIK, LEFT JOIN is shorthand for LEFT OUTER JOIN, so just pass through
  | outer_join_type OUTER { $$ = $1; }
// After some time looking at this, I can't seee any cases where this would be used
//  So it's in the BNF, but no implementations I could find allow for something
//  like SELECT * FROM table UNION JOIN table2; this is also the cause
//  of the conflict with non_join_query_expression. So this should remain
//  disabled until someone can demonstrate proper syntax
//  | UNION // This conflicts with non_join_query_expression
  ;

outer_join_type
  : RIGHT { SQL_STATEMENT($$, join_type_STATEMENT); ($$)->v.join_type = RIGHT_JOIN; }
  | LEFT { SQL_STATEMENT($$, join_type_STATEMENT); ($$)->v.join_type = LEFT_JOIN; }
  | FULL { SQL_STATEMENT($$, join_type_STATEMENT); ($$)->v.join_type = FULL_JOIN; }
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
