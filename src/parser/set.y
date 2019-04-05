sql_set_statement
  : SET identifier EQUALS literal_value {
      SQL_STATEMENT($$, set_STATEMENT);
    }
  ;
