insert_statement
  : INSERT INTO column_name query_expression
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression
  | INSERT INTO column_name DEFAULT VALUES
  ;
