/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
      assert(value_STATEMENT == ($literal_value)->type);
      if ((NUMERIC_LITERAL == ($literal_value)->v.value->type) || (INTEGER_LITERAL == ($literal_value)->v.value->type)) {
        SqlStatement    *ret;
        char            *c, *new_string;
        int             lit_int_val;
        float           lit_fl_val;

	$literal_value->v.value->type = INTEGER_LITERAL;	/* we will convert any fractions to integers so cast type */
        SQL_STATEMENT(ret, keyword_STATEMENT);
        OCTO_CMALLOC_STRUCT(ret->v.keyword, SqlOptionalKeyword);
        ret->v.keyword->keyword = OPTIONAL_LIMIT;
        ret->v.keyword->v = $literal_value;
        c = ret->v.keyword->v->v.value->v.string_literal;
        lit_fl_val = strtof(c, NULL);
        lit_int_val = (int)(lit_fl_val + 0.5);
        new_string = octo_cmalloc(memory_chunks, INT32_TO_STRING_MAX);
        snprintf(new_string, INT32_TO_STRING_MAX, "%d", lit_int_val);
        ret->v.keyword->v->v.value->v.string_literal = new_string;
        INVOKE_PARSE_LITERAL_TO_PARAMETER(parse_context, ret->v.keyword->v->v.value, TRUE);
        dqinit(ret->v.keyword);
        $$ = ret;
      } else {
        ERROR(ERR_INVALID_INPUT_SYNTAX, get_user_visible_type_string(($literal_value)->v.value->type));
        yyerror(NULL, NULL, &($literal_value), NULL, NULL, NULL);
        YYABORT;
      }
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
  : /* Empty */ { $$ = NULL; }
  | COMMA sort_specification_list { $$ = $sort_specification_list; }
  ;

sort_specification
  : sort_key { $$ = sort_specification($sort_key, NULL); }
  | sort_key ordering_specification { $$ = sort_specification($sort_key, $ordering_specification); }
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

      INVOKE_QUERY_SPECIFICATION(ret, (OptionalKeyword)$set_quantifier, $select_list, $table_expression,
      									$optional_order_by, plan_id);
      $$ = ret;
    }
  | SELECT set_quantifier select_list {
      // We're going to run against a secret table with one row so the list gets found
      SqlJoin			*join;
      SqlTable			*table;
      SqlStatement		*join_statement, *t_stmt, *select_list, *ret;
      SqlTableAlias		*alias;
      SqlSelectStatement	*select;
      char			*table_name = "OCTOONEROWTABLE";

      select_list = $select_list;
      if (NULL == select_list->v.column_list_alias->column_list) {
        ERROR(ERR_SELECT_STAR_NO_TABLES, NULL);
        yyerror(NULL, NULL, &select_list, NULL, NULL, NULL);
        YYERROR;
      }
      SQL_STATEMENT(t_stmt, select_STATEMENT);
      MALLOC_STATEMENT(t_stmt, select, SqlSelectStatement);
      UNPACK_SQL_STATEMENT(select, t_stmt, select);
      SQL_STATEMENT(join_statement, join_STATEMENT);
      MALLOC_STATEMENT(join_statement, join, SqlJoin);
      UNPACK_SQL_STATEMENT(join, join_statement, join);
      dqinit(join);
      join->max_unique_id = config->plan_id;
      table = find_table(table_name);
      if (NULL == table) {
        ERROR(ERR_UNKNOWN_TABLE, table_name);
        yyerror(NULL, NULL, &select_list, NULL, NULL, NULL);
        YYERROR;
      }
      SQL_STATEMENT(join->value, table_alias_STATEMENT);
      MALLOC_STATEMENT(join->value, table_alias, SqlTableAlias);
      UNPACK_SQL_STATEMENT(alias, join->value, table_alias);
      SQL_STATEMENT_FROM_SQLTABLE(alias, table);
      alias->alias = table->tableName;
      // We can probably put a variable in the bison local for this
      alias->unique_id = *plan_id;
      (*plan_id)++;
      select->table_list = join_statement;
      INVOKE_QUERY_SPECIFICATION(ret, (OptionalKeyword)$set_quantifier, select_list, t_stmt, NULL, plan_id);
      $$ = ret;
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
      SQL_COLUMN_LIST_ALIAS_STATEMENT($$);
      $$->loc = yyloc; /* note down the location of the ASTERISK for later use in populate_data_type (for error reporting) */
    }
  | derived_column { $$ = $derived_column; }
  ;

select_sublist_tail
  : /* Empty */ { $$ = NULL; }
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

	/* Traverse the all tables in the join list and ensure that each table has a unique alias. Else issue an error.
	 * Also use this opportunity to finish setting up the NATURAL JOIN condition (deferred in a "qualified_join" rule).
	 */
	UNPACK_SQL_STATEMENT(start_join, $$, join);
	cmp_join = start_join;
	do {
		SqlJoin		*cur_join;
		SqlTableAlias	*alias;
		SqlValue	*value;
		SqlStatement	*stmt;
		char		*cmp_name, *cur_name;

		if (NATURAL_JOIN == cmp_join->type) {
			/* cur_join->condition would not yet have been filled in (deferred in parser). Do that here and
			 * at the same time do some qualification checks too (errors will be returned as a non-zero value).
			 */
			if (natural_join_condition(start_join, cmp_join)) {
				YYERROR;
			}
		}
		stmt = drill_to_table_alias(cmp_join->value);
		UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
		UNPACK_SQL_STATEMENT(value, alias->alias, value);
		assert((COLUMN_REFERENCE == value->type) || (NUL_VALUE == value->type) || (STRING_LITERAL == value->type));
		cmp_name = value->v.string_literal;
		cur_join = cmp_join->next;
		while (cur_join != start_join) {
			stmt = drill_to_table_alias(cur_join->value);
			UNPACK_SQL_STATEMENT(alias, stmt, table_alias);
			UNPACK_SQL_STATEMENT(value, alias->alias, value);
			assert((COLUMN_REFERENCE == value->type) || (NUL_VALUE == value->type) || (STRING_LITERAL == value->type));
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
  : column_name {
	$$ = table_reference($column_name, NULL, plan_id);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | column_name correlation_specification {
	$$ = table_reference($column_name, $correlation_specification, plan_id);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | derived_table {
	$$ = $derived_table;
    }
  | joined_table { $$ = $joined_table; }
  ;

derived_table
  : table_subquery {
	$$ = derived_table($table_subquery, NULL);
	if (NULL == $$) {
		YYERROR;
	}
    }
  | table_subquery correlation_specification {
	$$ = derived_table($table_subquery, $correlation_specification);
	if (NULL == $$) {
		YYERROR;
	}
    }
  ;

table_reference_list_tail
  : /* Empty */ { $$ = NULL; }
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
  : IDENTIFIER_ALONE {
  	SqlStatement	*ret;

	ret = $IDENTIFIER_ALONE;
	ret->loc = yyloc;
	as_name(ret);
	$$ = ret;
      }
  | LITERAL {
	SqlStatement *ret;

	ret = $LITERAL;
	if ((NUMERIC_LITERAL == ret->v.value->type) || (INTEGER_LITERAL == ret->v.value->type) || (PARAMETER_VALUE == ret->v.value->type)) {
		ERROR(ERR_INVALID_AS_SYNTAX, get_user_visible_type_string(ret->v.value->type));
		yyerror(&yyloc, NULL, NULL, NULL, NULL, NULL);
		YYERROR;
	} else {
		as_name(ret);
	}
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
  : /* Empty */ { $$ = NULL; }
  | COMMA grouping_column_reference_list { $$ = $grouping_column_reference_list; }
  ;

grouping_column_reference
  : derived_column_expression {
	SqlStatement	*ret;

	$derived_column_expression->loc = yyloc;	// for later use by "grouping_column_reference()"
	ret = grouping_column_reference($derived_column_expression, NULL);
	if (NULL == ret) {
		/* GROUP BY specified using an expression. Issue error as this usage is not valid. */
		ERROR(ERR_GROUP_BY_ONLY_COLUMN_NAME, "");
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
		/* GROUP BY specified using an expression. Issue error as this usage is not valid. */
		ERROR(ERR_GROUP_BY_ONLY_COLUMN_NAME, "");
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
