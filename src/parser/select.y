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

sql_select_statement
  : query_specification optional_query_words {
      $$ = $query_specification;
      SqlTableAlias *table_alias;
      SqlSelectStatement *select;
      SqlOptionalKeyword *select_words, *new_words;
      UNPACK_SQL_STATEMENT(table_alias, $$, table_alias);
      UNPACK_SQL_STATEMENT(select, table_alias->table, select);
      UNPACK_SQL_STATEMENT(select_words, select->optional_words, keyword);
      UNPACK_SQL_STATEMENT(new_words, $optional_query_words, keyword);
      dqappend(select_words, new_words);
    }
  ;


optional_query_words
  : optional_query_word_element optional_query_word_tail {
      $$ = $optional_query_word_element;
      SqlOptionalKeyword *keyword;
      UNPACK_SQL_STATEMENT(keyword, $optional_query_word_tail, keyword);
      dqappend(keyword, ($$)->v.keyword);
    }
  | /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  ;

 optional_query_word_tail
  : /* Empty */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | COMMA optional_query_words { $$ = $COMMA; }
  ;

optional_query_word_element
  : LIMIT literal_value {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_LIMIT;
      ($$)->v.keyword->v = $literal_value;
      dqinit(($$)->v.keyword);
  }
/*  | UNION ALL sql_select_statement {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_UNION_ALL;
      ($$)->v.keyword->v = $sql_select_statement;
      dqinit(($$)->v.keyword);
  }
  | UNION sql_select_statement {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_UNION;
      ($$)->v.keyword->v = $sql_select_statement;
      dqinit(($$)->v.keyword);
      }*/
  ;

sort_specification_list
  : sort_specification sort_specification_list_tail {
      SqlColumnListAlias	*alias, *alias_tail;
      SqlValue			*value;
      SqlColumnList		*column_list;
      SqlStatement		*sort_specification;
      SqlSortSpecList		*sort_spec_list;

      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      dqinit(alias);
      assert(alias->next == alias);
      if ($sort_specification_list_tail) {
        UNPACK_SQL_STATEMENT(alias_tail, $sort_specification_list_tail, column_list_alias);
        dqappend(alias, alias_tail);
      }
      SQL_STATEMENT(alias->alias, value_STATEMENT);
      MALLOC_STATEMENT(alias->alias, value, SqlValue);
      UNPACK_SQL_STATEMENT(value, alias->alias, value);
      value->type = NUL_VALUE;
      value->v.string_literal = "";
      // Allocate the column list
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      // Get the allocated column list
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      dqinit(column_list);
      sort_specification = $sort_specification;
      // The sort spec should be a value column_REFERENCE
      UNPACK_SQL_STATEMENT(sort_spec_list, sort_specification, sort_spec_list);
      UNPACK_SQL_STATEMENT(value, sort_spec_list->column_value, value);
      assert(COLUMN_REFERENCE == value->type);
      column_list->value = sort_spec_list->column_value;
      alias->keywords = sort_spec_list->sort_type;
    }
  ;

sort_specification_list_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA sort_specification_list { $$ = $sort_specification_list; }
  ;

sort_specification
  : sort_key { $$ = sort_specification($sort_key, NULL, NULL); }
  | sort_key ordering_specification { $$ = sort_specification($sort_key, NULL, $ordering_specification); }
  ;

sort_key
  /// TODO: we somehow need to influence YottaDB's collation order
  : numeric_value_expression {
	SqlValue *value;
	/* if it is a value check if it is a column and return that otherwise issue a warning
	 * if it is not a value then it is some kind of expression so emit a warning
	*/
	if (value_STATEMENT == ($1)->type) {
		UNPACK_SQL_STATEMENT(value, $1, value);
		if (COLUMN_REFERENCE == value->type) {
			$$ = $1;
		} else if (COERCE_TYPE == value->type) {
			WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "ORDER BY typecast");
			YYABORT;
		} else {
			WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "ORDER BY column_number");
			YYABORT;
		}
	} else {
		WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "ORDER BY expression");
		YYABORT;
	}
    }
  ;

ordering_specification
  : ASC  {
      $$ = (YYSTYPE)OPTIONAL_ASC;
   }
  | DESC {
      $$ = (YYSTYPE)OPTIONAL_DESC;
   }
  ;

query_specification
  : SELECT set_quantifier select_list table_expression {
      $$ = query_specification($set_quantifier, $select_list, $table_expression, NULL, plan_id);
    }
  | SELECT set_quantifier select_list table_expression ORDER BY sort_specification_list {
      $$ = query_specification($set_quantifier, $select_list, $table_expression, $sort_specification_list, plan_id);
    }
  | SELECT set_quantifier select_list {
      // We're going to run against a secret table with one row so the list gets found
      SqlJoin			*join;
      SqlTable			*table;
      SqlStatement		*join_statement, *t_stmt;
      SqlTableAlias		*alias;
      SqlSelectStatement	*select;

      SQL_STATEMENT(t_stmt, select_STATEMENT);
      MALLOC_STATEMENT(t_stmt, select, SqlSelectStatement);
      UNPACK_SQL_STATEMENT(select, t_stmt, select);
      SQL_STATEMENT(join_statement, join_STATEMENT);
      MALLOC_STATEMENT(join_statement, join, SqlJoin);
      UNPACK_SQL_STATEMENT(join, join_statement, join);
      dqinit(join);
      table = find_table("OCTOONEROWTABLE");
      if (NULL == table) {
        ERROR(ERR_UNKNOWN_TABLE, "octoOneRowTable");
        print_yyloc(&($select_list)->loc);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT_FROM_TABLE_STATEMENT(alias->table, table);
      alias->table->v.table = table;
      alias->alias = table->tableName;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      select->table_list = join_statement;
      $$ = query_specification($set_quantifier, $select_list, t_stmt, NULL, plan_id);
    }
  ;

select_list
  : ASTERISK {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      $$->loc = yyloc; /* note down the location of the ASTERISK for later use in populate_data_type (for error reporting) */
    }
  | select_sublist { $$ = $select_sublist;  }
  ;

select_sublist
  : derived_column select_sublist_tail {
      $$ = $derived_column;
      // deviation from pattern here so we don't have to deal with "NOT_A_COLUMN" elsewhere
      if($select_sublist_tail != NULL) {
        SqlColumnListAlias *list1, *list2;
        UNPACK_SQL_STATEMENT(list1, $$, column_list_alias);
        UNPACK_SQL_STATEMENT(list2, $select_sublist_tail, column_list_alias);
        dqappend(list2, list1);
      }
    }
  ;

select_sublist_tail
  : /* Empty */ { $$ = NULL; }
  | COMMA select_sublist { $$ = $select_sublist; }
  ;

table_expression
  : from_clause where_clause group_by_clause having_clause {
      SQL_STATEMENT($$, select_STATEMENT);
      MALLOC_STATEMENT($$, select, SqlSelectStatement);
      ($$)->v.select->table_list = ($from_clause);
      ($$)->v.select->where_expression = $where_clause;
    }
  ;

set_quantifier
  : /* Empty; default ALL */ {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | ALL {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = NO_KEYWORD;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  | DISTINCT {
      SQL_STATEMENT($$, keyword_STATEMENT);
      OCTO_CMALLOC_STRUCT(($$)->v.keyword, SqlOptionalKeyword);
      ($$)->v.keyword->keyword = OPTIONAL_DISTINCT;
      ($$)->v.keyword->v = NULL;
      dqinit(($$)->v.keyword);
    }
  ;

derived_column
  : derived_column_expression {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      SqlColumnListAlias *alias;
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      dqinit(alias);
      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      dqinit(column_list);
      column_list->value = $derived_column_expression;
      /// TODO: we should search here for a reasonable "name" for the column
      alias->alias = find_column_alias_name($derived_column_expression);
      if(alias->alias == NULL) {
        SQL_STATEMENT(alias->alias, value_STATEMENT);
        MALLOC_STATEMENT(alias->alias, value, SqlValue);
        alias->alias->v.value->type = STRING_LITERAL;
        alias->alias->v.value->v.string_literal = octo_cmalloc(memory_chunks, strlen("???") + 2);
        strcpy(alias->alias->v.value->v.string_literal, "???");
      }
      alias->column_list->loc = yyloc;
    }
  | derived_column_expression AS column_name {
      SQL_STATEMENT($$, column_list_alias_STATEMENT);
      MALLOC_STATEMENT($$, column_list_alias, SqlColumnListAlias);
      SqlColumnListAlias *alias;
      UNPACK_SQL_STATEMENT(alias, $$, column_list_alias);
      SQL_STATEMENT(alias->column_list, column_list_STATEMENT);
      dqinit(alias);
      MALLOC_STATEMENT(alias->column_list, column_list, SqlColumnList);
      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, alias->column_list, column_list);
      dqinit(column_list);
      column_list->value = $derived_column_expression;
      alias->alias = $column_name;
      alias->column_list->loc = yyloc;
    }
  ;

derived_column_expression
  : value_expression { $$ = $value_expression; }
  | search_condition { $$ = $search_condition; }
  ;

from_clause
  : FROM table_reference {
      SqlJoin *start_join, *cmp_join, *cur_join;
      SqlTableAlias *alias;
      SqlValue *value;
      SqlStatement *stmt;
      char *cmp_name, *cur_name;
      $$ = $table_reference;
      /* traverse the entire table linked list and compare the table_alias
       * this ensures each table has a unique alias
      */
      UNPACK_SQL_STATEMENT(start_join, $$, join);
      cmp_join = start_join;
      do {
        stmt = drill_to_table_alias(cmp_join->value);
        UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
        UNPACK_SQL_STATEMENT(value, alias->alias, value);
        assert((COLUMN_REFERENCE == value->type) || (NUL_VALUE == value->type));
        cmp_name = value->v.string_literal;
        cur_join = cmp_join->next;
        while (cur_join != start_join) {
          stmt = drill_to_table_alias(cur_join->value);
          UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
          UNPACK_SQL_STATEMENT(value, alias->alias, value);
          assert((COLUMN_REFERENCE == value->type) || (NUL_VALUE == value->type));
          cur_name = value->v.string_literal;
          if (0 == strcmp(cmp_name, cur_name)) {
            ERROR(ERR_JOIN_ALIAS_DUPLICATE, cmp_name);
            YYABORT;
          }
          cur_join = cur_join->next;
        }
        cmp_join = cmp_join->next;
      } while (cmp_join != start_join);
   }
  ;

// Just consider these a list of values for all intensive purposes
table_reference
  : column_name table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlTable *table = find_table($column_name->v.value->v.reference);
      SqlJoin *join = $$->v.join, *join_tail;
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
      SQL_STATEMENT_FROM_TABLE_STATEMENT(alias->table, table);
      alias->table->v.table = table;
      alias->alias = table->tableName;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      UNPACK_SQL_STATEMENT(column, table->columns, column);
      PACK_SQL_STATEMENT(alias->column_list,
                         columns_to_column_list_alias(column, alias), column_list_alias);
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        join->type = CROSS_JOIN;
        dqappend(join, join_tail);
      }
    }
  | column_name correlation_specification table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlTable *table = find_table($column_name->v.value->v.reference);
      SqlJoin *join = $$->v.join, *join_tail;
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
      SQL_STATEMENT_FROM_TABLE_STATEMENT(alias->table, table);
      alias->table->v.table = table;
      alias->alias = $correlation_specification;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      UNPACK_SQL_STATEMENT(column, table->columns, column);
      PACK_SQL_STATEMENT(alias->column_list,
                         columns_to_column_list_alias(column, alias), column_list_alias);
      dqinit(join);
      if($table_reference_tail) {
        UNPACK_SQL_STATEMENT(join_tail, $table_reference_tail, join);
        join->type = CROSS_JOIN;
        dqappend(join, join_tail);
      }
    }
  | derived_table {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlJoin *join = $$->v.join;
      join->value = $derived_table;
      dqinit(join);
    }
  | derived_table correlation_specification {
      SqlTableAlias	*table_alias;
      SqlStatement	*sql_stmt;

      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      SqlJoin *join = $$->v.join;
      sql_stmt = $derived_table;
      join->value = sql_stmt;

      // Setup the alias
      sql_stmt = drill_to_table_alias(sql_stmt);
      UNPACK_SQL_STATEMENT(table_alias, sql_stmt, table_alias);
      table_alias->alias = $correlation_specification;
      dqinit(join);
    }
  | joined_table { $$ = $joined_table; }
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
  : table_subquery {$$ = $table_subquery; }
  ;

joined_table
  : cross_join { $$ = $cross_join; }
  | qualified_join { $$ = $qualified_join; }
  | LEFT_PAREN joined_table RIGHT_PAREN { $$ = $2; }
  ;

cross_join
  : table_reference CROSS JOIN table_reference {
      SqlJoin *left, *right;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      left->type = CROSS_JOIN;
      dqappend(left, right);
    }
  ;

qualified_join
  : table_reference JOIN table_reference join_specification {
      SqlJoin *left, *right;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $3, join);
      right->type = INNER_JOIN;
      right->condition = $join_specification;
      dqappend(left, right);
    }
  | table_reference NATURAL JOIN table_reference optional_join_specification {
      SqlJoin *left, *right;
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
      dqappend(left, right);
    }

  | table_reference join_type JOIN table_reference join_specification {
      SqlJoin *left, *right;
      $$ = $1;
      UNPACK_SQL_STATEMENT(left, $$, join);
      UNPACK_SQL_STATEMENT(right, $4, join);
      UNPACK_SQL_STATEMENT(right->type, $join_type, join_type);
      right->condition = $join_specification;
      dqappend(left, right);
    }
  | table_reference NATURAL join_type JOIN table_reference join_specification
  ;

optional_join_specification
  : /* Empty */ { $$ = NULL; }
  | join_specification { $$ = $join_specification; }
  ;

join_specification
  : join_condition { $$ = $join_condition; }
  | named_column_joins
  ;

named_column_joins
  : USING LEFT_PAREN join_column_list RIGHT_PAREN
  ;

join_column_list
  : column_name_list
  ;

join_condition
  : ON search_condition { $$ = $search_condition; }
  ;

join_type
  : INNER { SQL_STATEMENT($$, join_type_STATEMENT); ($$)->v.join_type = INNER_JOIN; }
  | outer_join_type { $$ = $outer_join_type; }
// AFAIK, LEFT JOIN is shorthand for LEFT OUTER JOIN, so just pass through
  | outer_join_type OUTER { $$ = $outer_join_type; }
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
