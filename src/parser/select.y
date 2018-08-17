sql_select_statement
  : query_specification
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
  | UNSIGNED_INTEGER
  ;

ordering_specification
  : ASC
  | DESC
  ;

query_specification
  : SELECT select_list table_expression
  | SELECT set_quantifier select_list table_expression
  | SELECT select_list table_expression ORDER BY sort_specification_list
  | SELECT set_quantifier select_list table_expression ORDER BY sort_specification_list
  ;

select_list
  : ASTERISK
  | select_sublist
  ;

select_sublist
  : derived_column
  | derived_column select_sublist_tail
  ;

select_sublist_tail
  : COMMA select_sublist
  ;

table_expression
  : from_clause where_clause group_by_clause having_clause
  ;

set_quantifier
  : ALL
  | DISTINCT
  ;

/* !!! deviations from standard; to return an "expression" as a column,
    it must be surrounded by parens
*/
derived_column
  : non_query_value_expression
  | non_query_value_expression AS column_name
  ;

from_clause
  : FROM table_reference
  ;

table_reference
  : column_name table_reference_tail
  | column_name correlation_specification table_reference_tail
  | derived_table
  | derived_table correlation_specification
  | joined_table
  ;

table_reference_tail
  : /* Empty */
  | COMMA table_reference
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
  : table_subquery
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
  : /* Empty */
  | WHERE search_condition
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
