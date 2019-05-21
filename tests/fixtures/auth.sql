CREATE TABLE auth (id INTEGER PRIMARY KEY, userName VARCHAR(30), password VARCHAR(30), pwHash VARCHAR(16), salt VARCHAR(8));
-- CREATE TABLE namesWithAges (id INTEGER PRIMARY KEY, fistName VARCHAR(30), lastName VARCHAR(30), age INTEGER) GLOBAL "^names(keys(""id""))";
