insert_statement
  : INSERT INTO column_name subquery { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name subquery"); YYABORT; }
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression"); YYABORT; }
  | INSERT INTO column_name DEFAULT VALUES { WARNING(ERR_FEATURE_NOT_IMPLEMENTED, "insert_statement: INSERT INTO column_name DEFAULT VALUES"); YYABORT; }
  ;
