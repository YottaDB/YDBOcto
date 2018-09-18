sql_select_statement
  : query_specification { $$ = $1; }
  ;

sort_specification_list
  : sort_specification sort_specification_list_tail
  ;

sort_specification_list_tail
  : /* Empty */
  | COMMA sort_specification_list
  ;

sort_specification
  : sort_key
  | sort_key collate_clause
  | sort_key ordering_specification
  | sort_key collate_clause ordering_specification
  ;

sort_key
  : column_reference
  | LITERAL
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
    }
  | SELECT set_quantifier select_list table_expression
  | SELECT select_list table_expression ORDER BY sort_specification_list
  | SELECT set_quantifier select_list table_expression ORDER BY sort_specification_list
  ;

select_list
  : ASTERISK {
      SQL_STATEMENT($$, column_list_STATEMENT);
      ($$)->v.column_list = 0;
    }
  | select_sublist { $$ = $1;  }
  ;

select_sublist
  : derived_column select_sublist_tail {
      $$ = $1;
      ($$)->v.column_list->next = $2;
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
    }
  ;

set_quantifier
  : ALL
  | DISTINCT
  ;

derived_column
  : non_query_value_expression {
      SQL_STATEMENT($$, column_list_STATEMENT);
      assert(($1)->type == value_STATEMENT);
      ($$)->v.column_list = (SqlColumnList*)malloc(sizeof(SqlColumnList));
      ($$)->v.column_list->next = 0;
      ($$)->v.column_list->value = $1;
    }
  | non_query_value_expression AS column_name
  ;

from_clause
  : FROM table_reference {$$ = $2; }
  ;

// Just consider these a list of values for all intensive purposes
table_reference
  : column_name table_reference_tail {
      SQL_STATEMENT($$, join_STATEMENT);
      ($$)->v.join = (SqlJoin*)malloc(sizeof(SqlJoin));
      ($$)->v.join->value = $1;
      ($$)->v.join->next = ($2);
    }
  | column_name correlation_specification table_reference_tail
  | derived_table {
      SQL_STATEMENT($$, join_STATEMENT);
      MALLOC_STATEMENT($$, join, SqlJoin);
      ($$)->v.join->type = TABLE_SPEC;
      ($$)->v.join->value = $1;
    }
  | derived_table correlation_specification
  | joined_table
  ;

table_reference_tail
  : /* Empty */ {
      $$ = (SqlStatement*)malloc(sizeof(SqlStatement));
      ($$)->type = join_STATEMENT;
      ($$)->v.join = 0;
    }
  | COMMA table_reference { $$ = $1; }
  ;

correlation_specification
  : optional_as column_name
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
  : cross_join
  | qualified_join
  | LEFT_PAREN joined_table RIGHT_PAREN
  ;

cross_join
  : table_reference CROSS JOIN table_reference
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
