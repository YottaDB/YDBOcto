
==============================
Programming Notes
==============================

.. contents::
   :depth: 2

+++++++++++++++++++++
Empty string and NULL
+++++++++++++++++++++

Currently, in YottaDB globals there is no distinction between an empty (zero length) string and NULL (the absence of a value). By default all empty strings are zero length strings for columns having a character value, and zero (0) for columns specified as having numeric values.

For example,

.. code-block:: PSQL

   SELECT * FROM Employee WHERE FirstName IS NULL;

and

.. code-block:: PSQL

   SELECT * FROM Employee WHERE FirstName = "";

return the same results.

