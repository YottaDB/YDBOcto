CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
CREATE TABLE namesWithAges (id INTEGER PRIMARY KEY, fistName VARCHAR(30), lastName VARCHAR(30), age INTEGER) GLOBAL "^names(keys(""id""))";
