UPDATE myTable
SET id = 3, name = "Orion"
WHERE breed = "Black Lab";
UPDATE myTable
SET name = (SELECT name FROM tableOfDogs WHERE breed = "Black lab");
