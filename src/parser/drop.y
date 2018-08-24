drop_table_statement
  : DROP TABLE column_name DROP_BEHAVIOR {
      SQL_STATEMENT($$, drop_STATEMENT);
      ($$)->v.drop = (SqlDropStatement*)malloc(sizeof(SqlDropStatement));
      ($$)->v.drop->table_name = $column_name;
      ($$)->v.drop->optional_keyword = $DROP_BEHAVIOR;
    }
  ;

DROP_BEHAVIOR
  : /* Empty */ { $$ = NULL; }
  | CASCADE {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_CASCADE;
      ($$)->v.keyword->v = NULL;
    }
  | RESTRICT {
      SQL_STATEMENT($$, keyword_STATEMENT);
      ($$)->v.keyword = (SqlOptionalKeyword*)malloc(sizeof(SqlOptionalKeyword));
      ($$)->v.keyword->keyword = OPTIONAL_RESTRICT;
      ($$)->v.keyword->v = NULL;
    }
  ;
