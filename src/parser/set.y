sql_set_statement
  : SET identifier EQUALS literal_value {
      SqlSetStatement *set;
      SQL_STATEMENT($$, set_STATEMENT);
      MALLOC_STATEMENT($$, set, SqlSetStatement);
      UNPACK_SQL_STATEMENT(set, $$, set);
      set->variable = $identifier;
      set->value = $literal_value;
    }
  | SET identifier TO literal_value {
      SqlSetStatement *set;
      SQL_STATEMENT($$, set_STATEMENT);
      MALLOC_STATEMENT($$, set, SqlSetStatement);
      UNPACK_SQL_STATEMENT(set, $$, set);
      set->variable = $identifier;
      set->value = $literal_value;
    }
  | SHOW identifier {
      SqlShowStatement *show;
      SQL_STATEMENT($$, show_STATEMENT);
      MALLOC_STATEMENT($$, show, SqlShowStatement);
      UNPACK_SQL_STATEMENT(show, $$, show);
      show->variable = $identifier;
    }
  ;
