insert_statement
  : INSERT INTO column_name subquery {
      SQL_STATEMENT($$, insert_STATEMENT);
      MALLOC_STATEMENT($$, insert, SqlInsertStatement);
      SqlValue *value = NULL;
      SqlSelectStatement *select;
      UNPACK_SQL_STATEMENT(select, $subquery, select);
      UNPACK_SQL_STATEMENT(value, $column_name, value);
      SqlTable *destination = find_table(value->v.reference);
      if(destination == NULL) {
        ERROR(ERR_UNKNOWN_TABLE, value->v.reference);
        print_yyloc(&($column_name)->loc);
        YYABORT;
      }
      /* By default, user to specify all columns */
      ($$)->v.insert->columns = NULL;
      ($$)->v.insert->destination = destination;
      ($$)->v.insert->source = $subquery;
    }
  | INSERT INTO column_name LEFT_PAREN column_name_list RIGHT_PAREN query_expression
  | INSERT INTO column_name DEFAULT VALUES
  ;
