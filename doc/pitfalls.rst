.. #################################################################
.. #								   #
.. # Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.  #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

==========
Pitfalls
==========

.. contents::
   :depth: 2

-----------------------------------
Fixing out of sync cross reference
-----------------------------------

  To speed up query execution, Octo automatically builds cross references as needed. It has been observed that in rare cases (see `YottaDB/DB/YDB/#659 <https://gitlab.com/YottaDB/DB/YDB/-/issues/659>`_ for details) the cross references can get out of sync. If you encounter such a situation, run the following command to rebuild the cross reference.

  If an Octo version before 84de9324 is used (Octo maintained its own reference before this commit) then for a tablename :code:`names` and column name :code:`firstname` run the following

  .. code-block:: bash

     $ $ydb_dist/yottadb -run %XCMD 'KILL ^%ydboctoocto("xref_status","names","firstname")'

  If Octo version is or after 84de9324 (YDBAIM maintains the cross reference for Octo) then for a tablename :code:`names` and column name :code:`firstname` run the following

  .. code-block:: bash

     $ $ydb_dist/yottadb -run %XCMD 'DO UNXREFDATA^%YDBAIM($QSUBSCRIPT(^%ydbAIMOctoCache("names","firstname","location"),0)) KILL ^%ydbAIMOctoCache("names","firstname")'

  Run your original query, in Octo, to rebuild the cross reference.

.. note::
   When running the YDBAIM specific command above if Octo is executing SELECT queries concurrently, it is possible a SELECT sneaks in between the UNXREFDATA and the KILL commands. This can result in SELECT query to return incorrect results.
