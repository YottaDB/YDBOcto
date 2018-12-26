update_statement_searched
  : UPDATE column_name SET set_clause_list { $$ = NULL; }
  | UPDATE column_name SET set_clause_list WHERE search_condition { $$ = NULL; }
  ;

set_clause_list
  : set_clause set_clause_tail
  ;

set_clause_tail
  : /* Empty */
  | COMMA set_clause_list
  ;

set_clause
  : object_column EQUALS update_source
  ;

object_column
  : column_name
  ;

update_source
  : value_expression
  | null_specification
  | DEFAULT
  ;
