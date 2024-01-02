/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
      $$ = alloc_keyword_of_type(NO_KEYWORD);
    }
  ;

 optional_query_word_tail
  : /* Empty */ {
      $$ = alloc_keyword_of_type(NO_KEYWORD);
    }
  ;

optional_query_word_element
  : LIMIT literal_value {
      assert(value_STATEMENT == ($literal_value)->type);
      if ((NUMERIC_LITERAL == ($literal_value)->v.value->type) || (INTEGER_LITERAL == ($literal_value)->v.value->type)) {
        SqlStatement    *ret;
        char            *c, *new_string;
        int             lit_int_val;
        float           lit_fl_val;

	$literal_value->v.value->type = INTEGER_LITERAL;	/* we will convert any fractions to integers so cast type */
	MALLOC_KEYWORD_STMT(ret, OPTIONAL_LIMIT);
        ret->v.keyword->v = $literal_value;
        c = ret->v.keyword->v->v.value->v.string_literal;
        lit_fl_val = strtof(c, NULL);
        lit_int_val = (int)(lit_fl_val + 0.5);
        new_string = octo_cmalloc(memory_chunks, INT32_TO_STRING_MAX);
        snprintf(new_string, INT32_TO_STRING_MAX, "%d", lit_int_val);
        ret->v.keyword->v->v.value->v.string_literal = new_string;
        INVOKE_PARSE_LITERAL_TO_PARAMETER(parse_context, ret->v.keyword->v->v.value, TRUE);
        $$ = ret;
      } else {
        ERROR(ERR_INVALID_INPUT_SYNTAX, get_user_visible_type_string(($literal_value)->v.value->type));
        yyerror(NULL, NULL, &($literal_value), NULL, NULL, NULL);
        YYABORT;
      }
    }

sort_specification_list
  : sort_specification sort_specification_list_tail {
	SqlStatement		*sort_spec, *sort_spec_tail;

	sort_spec = $sort_specification;
	sort_spec_tail = $sort_specification_list_tail;
	if (NULL != sort_spec_tail) {
		SqlColumnListAlias	*list1, *list2;

		UNPACK_SQL_STATEMENT(list1, sort_spec, column_list_alias);
		UNPACK_SQL_STATEMENT(list2, sort_spec_tail, column_list_alias);
		dqappend(list2, list1);
	}
	$$ = sort_spec;
    }
  ;

sort_specification_list_tail
  : /* Empty */ { $$ = NULL; }		%prec PREC1
  | COMMA sort_specification_list { $$ = $sort_specification_list; }
  ;

sort_specification
  : sort_key {
	SqlStatement *result = sort_specification($sort_key, NULL);
	if (NULL == result) {
		YYERROR;
	}
	$$ = result;
    }
  | sort_key ordering_specification {
	SqlStatement *result = sort_specification($sort_key, $ordering_specification);
	if (NULL == result) {
		YYERROR;
	}
	$$ = result;
    }
  ;

sort_key
  /// TODO: we somehow need to influence YottaDB's collation order
  : derived_column_expression {
	$$ = $derived_column_expression;
	$$->loc = yyloc;	// for later use by "sort_specification()"
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
  : SELECT set_quantifier select_list table_expression optional_order_by {
     SqlStatement *ret;

      INVOKE_QUERY_SPECIFICATION(ret, (OptionalKeyword)(uintptr_t)$set_quantifier, $select_list, $table_expression,
      									$optional_order_by, plan_id);
      $$ = ret;
      $$->loc = @SELECT;	/* useful for error reporting to know lexical start of query */
    }
  | SELECT set_quantifier select_list where_clause group_by_clause having_clause optional_order_by {
      // We're going to run against a secret table with one row so the list gets found
      SqlJoin			*join;
      SqlStatement		*join_statement, *select_list, *ret, *table_or_view_stmt;
      SqlTableAlias		*alias;
      char			*table_name = OCTOLIT_OCTOONEROWTABLE;
      SqlColumnListAlias        *start_cla, *cur_cla;

      select_list = $select_list;
      UNPACK_SQL_STATEMENT(start_cla, select_list, column_list_alias);
      cur_cla = start_cla;
      do {
	SqlColumnList	*column_list;
	SqlValue	*value;

        UNPACK_SQL_STATEMENT(column_list, cur_cla->column_list, column_list);
	if (value_STATEMENT == column_list->value->type) {
	  UNPACK_SQL_STATEMENT(value, column_list->value, value);
	  if (SELECT_ASTERISK == value->type) {
	    /* We need to issue an ERR_SELECT_STAR_NO_TABLES error here. But doing so could issue misleading errors in case
	     * the query has a syntax error (see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1378#note_1379104917
	     * for more details). Therefore, we defer issuing this error until "qualify_query()" time as by that time,
	     * the full line would be parsed and any syntax errors in the query would be issued by then. We record this by
	     * storing a NULL pointer in "value->v.string_literal".
	     */
	    assert(NULL != value->v.string_literal);
	    value->v.string_literal = NULL;
          }
	}
        cur_cla = cur_cla->next;
      } while (cur_cla != start_cla);
      SQL_STATEMENT(join_statement, join_STATEMENT);
      MALLOC_STATEMENT(join_statement, join, SqlJoin);
      UNPACK_SQL_STATEMENT(join, join_statement, join);
      dqinit(join);
      join->max_unique_id = config->plan_id;
      table_or_view_stmt = find_view_or_table(table_name);
      if (NULL == table_or_view_stmt) {
        ERROR(ERR_UNKNOWN_TABLE, table_name);
        yyerror(NULL, NULL, &select_list, NULL, NULL, NULL);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      if (create_view_STATEMENT == table_or_view_stmt->type) {
      	SqlView *view;
        UNPACK_SQL_STATEMENT(view, table_or_view_stmt, create_view);
        SQL_STATEMENT_FROM_SQLTABLE_OR_SQLVIEW(alias, view);
	alias->alias = view->viewName;
      } else {
      	SqlTable *table;
	UNPACK_SQL_STATEMENT(table, table_or_view_stmt, create_table);
        SQL_STATEMENT_FROM_SQLTABLE_OR_SQLVIEW(alias, table);
        alias->alias = table->tableName;
      }
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;

      SqlStatement *t_stmt = table_expression(join_statement, $where_clause, $group_by_clause, $having_clause);
      INVOKE_QUERY_SPECIFICATION(ret, (OptionalKeyword)(uintptr_t)$set_quantifier, select_list, t_stmt, $optional_order_by, plan_id);
      $$ = ret;
      $$->loc = @SELECT;	/* useful for error reporting to know lexical start of query */
    }
  ;

select_list
  : select_sublist select_sublist_tail {
	SqlStatement *select_list;

	select_list = $select_sublist;
	if (NULL != $select_sublist_tail) {
		SqlColumnListAlias	*list1, *list2;

		UNPACK_SQL_STATEMENT(list1, select_list, column_list_alias);
		UNPACK_SQL_STATEMENT(list2, $select_sublist_tail, column_list_alias);
		dqappend(list1, list2);
	}
	$$ = select_list;
    }
  ;

select_sublist
  : ASTERISK {
      SqlStatement	*ret;

      SQL_COLUMN_LIST_ALIAS_STATEMENT(ret);

      SqlColumnListAlias	*cla;
      UNPACK_SQL_STATEMENT(cla, ret, column_list_alias);
      SQL_COLUMN_LIST_STATEMENT(cla->column_list);

      SqlColumnList *column_list;
      UNPACK_SQL_STATEMENT(column_list, cla->column_list, column_list);
      SQL_VALUE_STATEMENT(column_list->value, SELECT_ASTERISK, ""); /* note down the location of the ASTERISK for later use
								     * in populate_data_type (for error reporting).
								     */
      cla->column_list->loc = yyloc; /* note down location of ASTERISK for later use in populate_data_type (for error reporting) */
      $$ = ret;
    }
  | derived_column { $$ = $derived_column; }
  ;

select_sublist_tail
  : /* Empty */ { $$ = NULL; }			%prec PREC1
  | COMMA select_list { $$ = $select_list; }
  ;

table_expression
  : from_clause where_clause group_by_clause having_clause {
	$$ = table_expression($from_clause, $where_clause, $group_by_clause, $having_clause);
    }
  ;

set_quantifier
  : /* Empty; default ALL */ {
      $$ = (SqlStatement *)NO_KEYWORD;
    }
  | ALL {
      $$ = (SqlStatement *)NO_KEYWORD;
    }
  | DISTINCT {
      $$ = (SqlStatement *)OPTIONAL_DISTINCT;
    }
  ;

derived_column
  : derived_column_expression {
	$$ = derived_column($derived_column_expression, NULL, &yyloc);
    }
  | derived_column_expression optional_as as_name {
        $$ = derived_column($derived_column_expression, $as_name, &yyloc);
    }
  ;

derived_column_expression
  : value_expression { $$ = $value_expression; }
  ;

from_clause
  : FROM table_reference_list {
	SqlJoin		*start_join, *cmp_join;


	$$ = $table_reference_list;

	/* Traverse the all tables in the join list and ensure that each table has a unique alias. Else issue an error. */
	UNPACK_SQL_STATEMENT(start_join, $$, join);
	cmp_join = start_join;
	do {
		SqlJoin		*cur_join;
		SqlTableAlias	*alias;
		SqlValue	*value;
		SqlStatement	*stmt;
		char		*cmp_name, *cur_name;

		stmt = drill_to_table_alias(cmp_join->value);
		UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
		UNPACK_SQL_STATEMENT(value, alias->alias, value);
		assert((COLUMN_REFERENCE == value->type) || IS_NUL_VALUE(value->type)
			 || (STRING_LITERAL == value->type));
		cmp_name = value->v.string_literal;
		cur_join = cmp_join->next;
		while (cur_join != start_join) {
			stmt = drill_to_table_alias(cur_join->value);
			UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
			UNPACK_SQL_STATEMENT(value, alias->alias, value);
			assert((COLUMN_REFERENCE == value->type) || IS_NUL_VALUE(value->type)
				|| (STRING_LITERAL == value->type));
			cur_name = value->v.string_literal;
			if (0 == strcmp(cmp_name, cur_name)) {
				ERROR(ERR_JOIN_ALIAS_DUPLICATE, cmp_name);
				YYERROR;
			}
			cur_join = cur_join->next;
		}
		cmp_join = cmp_join->next;
	} while (cmp_join != start_join);
   }
  ;

table_reference_list
  : table_reference table_reference_list_tail {
	SqlJoin  *join_tail, *join;
	if (NULL != $table_reference_list_tail) {
		join = ($table_reference)->v.join;
		// No need for dqinit(join) here as
		// already taken care by table_reference rule
		UNPACK_SQL_STATEMENT(join_tail, $table_reference_list_tail, join);
		join_tail->type = CROSS_JOIN;
		dqappend(join, join_tail);
	}
	$$ = $table_reference;
  }
  ;
// Just consider these a list of values for all intents and purposes
table_reference
  : qualified_name {
	INVOKE_TABLE_REFERENCE($$, $qualified_name, NULL, plan_id);
    }
  | qualified_name correlation_specification {
	INVOKE_TABLE_REFERENCE($$, $qualified_name, $correlation_specification, plan_id);
    }
  | derived_table {
	$$ = $derived_table;
    }
  | joined_table { $$ = $joined_table; }
  ;

derived_table
  : table_subquery {
	INVOKE_DERIVED_TABLE($$, $table_subquery, NULL);
    }
  | table_subquery correlation_specification {
	INVOKE_DERIVED_TABLE($$, $table_subquery, $correlation_specification);
    }
  ;

table_reference_list_tail
  : /* Empty */ { $$ = NULL; }		%prec PREC1
  | COMMA table_reference_list { $$ = $table_reference_list; }
  ;

correlation_specification
  : optional_as as_name { $$ = create_sql_column_list($as_name, NULL, &yyloc); }
  | optional_as as_name LEFT_PAREN column_name_list RIGHT_PAREN
    {
	$$ = create_sql_column_list($as_name, $column_name_list, &yyloc);
    }
  ;

optional_as
  : /* Empty */
  | AS
  ;

as_name
  : column_name {
  	SqlStatement	*ret;

	ret = $column_name;
	ret->loc = yyloc;
	assert(value_STATEMENT == ret->type);
	assert(OCTO_MAX_IDENT >= strlen(ret->v.value->v.string_literal));
	/* SqlValue type of "as_name" is set to "STRING_LITERAL" in order to prevent multiple plan generation
	 * for queries differing only by alias name or LITERAL value.
	 */
	ret->v.value->type = STRING_LITERAL;
	$$ = ret;
      }
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
      right->type = CROSS_JOIN;
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
  | table_reference NATURAL JOIN table_reference {
	SqlJoin		*left, *right;

	$$ = $1;
	UNPACK_SQL_STATEMENT(left, $$, join);
	UNPACK_SQL_STATEMENT(right, $4, join);
	right->type = NATURAL_JOIN;
	/* right->condition will be filled in at a later point as part of a call to "natural_join_condition()".
	 * Calling it here does not work since it expects to process tables in the join list from left to right
	 * whereas the current grammar rules causes it to be invoked from the rightmost table to the leftmost table
	 * in the join list if it is invoked here.
	 */
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
  | table_reference NATURAL join_type JOIN table_reference join_specification {
	ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "NATURAL INNER|LEFT|RIGHT|FULL JOIN"); YYABORT;
    }
  ;

join_specification
  : join_condition { $$ = $join_condition; }
  | named_column_joins
  ;

named_column_joins
  : USING LEFT_PAREN join_column_list RIGHT_PAREN { ERROR(ERR_FEATURE_NOT_IMPLEMENTED, "USING clause"); YYABORT; }
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
  : /* Empty */ { $$ = NULL; }
  | GROUP BY grouping_column_reference_list { $$ = $grouping_column_reference_list; }
  ;

grouping_column_reference_list
  : grouping_column_reference grouping_column_reference_list_tail	{
	if (NULL != $grouping_column_reference_list_tail) {
		SqlColumnListAlias	*group_by_head, *group_by_tail;

		UNPACK_SQL_STATEMENT(group_by_head, $grouping_column_reference, column_list_alias);
		UNPACK_SQL_STATEMENT(group_by_tail, $grouping_column_reference_list_tail, column_list_alias);
		dqappend(group_by_head, group_by_tail);
	}
	$$ = $grouping_column_reference;
    }
  ;

grouping_column_reference_list_tail
  : /* Empty */ { $$ = NULL; }		%prec PREC1
  | COMMA grouping_column_reference_list { $$ = $grouping_column_reference_list; }
  ;

grouping_column_reference
  : derived_column_expression {
	SqlStatement	*ret;

	$derived_column_expression->loc = yyloc;	// for later use by "grouping_column_reference()"
	ret = grouping_column_reference($derived_column_expression, NULL);
	if (NULL == ret) {
		/* Issue error as this usage is not valid. */
		ERROR(ERR_GROUP_BY_INVALID_USAGE, "");
		yyerror(NULL, NULL, &($derived_column_expression), NULL, NULL, NULL);
		YYERROR;
	}
	$$ = ret;
    }
  | derived_column_expression collate_clause {
	SqlStatement	*ret;

	$derived_column_expression->loc = yyloc;	// for later use by "grouping_column_reference()"
	ret = grouping_column_reference($derived_column_expression, $collate_clause);
	if (NULL == ret) {
		/* Issue error as this usage is not valid. */
		ERROR(ERR_GROUP_BY_INVALID_USAGE, "");
		yyerror(NULL, NULL, &($derived_column_expression), NULL, NULL, NULL);
		YYERROR;
	}
	$$ = ret;
    }
  ;

having_clause
  : /* Empty */	{ $$ = NULL; }
  | HAVING search_condition { $$ = $search_condition; }
  ;
