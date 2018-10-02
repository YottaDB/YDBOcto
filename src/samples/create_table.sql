CREATE TABLE abc (id INTEGER PRIMARY KEY);
CREATE TABLE myTable (id INTEGER PRIMARY KEY);
CREATE TABLE myTable (id INT PRIMARY KEY, name VARCHAR(255));
CREATE TABLE tableWithManyFields (
  id INTEGER PRIMARY KEY,
  name CHARACTER(255),
  gender CHAR,
  weight DECIMAL(3) NOT NULL
);
CREATE TABLE abc (id INTEGER PRIMARY KEY);
CREATE TABLE abc (id INTEGER PRIMARY KEY);
create table abc (id INTEGER PRIMARY KEY) PACK "$$PACK(%s,.keys)" UNPACK "$$UNPACK(%s,.keys)";
CREATE TABLE abc (id INTEGER PRIMARY KEY PIECE "keys(0)");
CREATE TABLE abc (id INTEGER PRIMARY KEY PIECE "keys(0)" GLOBAL "^aDifferentVar(keys(0))");
CREATE TABLE abc (
  id INTEGER PRIMARY KEY EXTRACT "abc"
  GLOBAL "someGLobal"
  PIECE "0"
  DELIM "|"
) CURSOR "$$CURSE" DELIM "|"
END "keys(0)" GLOBAL "someGLobal"
PACK "$$PACK" UNPACK "$$UNPACK" ;
create table abc (id integer primary key, oid integer key num 1, name varchar(30), oid2 integer key num 2);
