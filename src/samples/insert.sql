INSERT INTO myTable VALUES (1, 2, "Name");
INSERT INTO myTable (id, name) VALUES (1, "Emma");
INSERT INTO myTable (id, name) VALUES
  (1, "Emma"),
  (2, "Jojo"),
  (3, "Max")
;
INSERT INTO myTable2
SELECT * from myTable3;
INSERT INTO theTable DEFAULT VALUES;
