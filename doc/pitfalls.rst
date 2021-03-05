.. #################################################################
.. #								   #
.. # Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.  #
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

+++++++++++++++++++++++++++++++++++
Fixing out of sync cross reference
+++++++++++++++++++++++++++++++++++

To speed up query execution, Octo automatically builds cross references for every *<table_name, column_name>* pair as needed. It has been observed that in rare cases (see `YottaDB/DB/YDB/#659 <https://gitlab.com/YottaDB/DB/YDB/-/issues/659>`_ for details) the cross references can get out of sync. If you encounter such a situation, run the following command to rebuild the cross reference for a particular *<table_name, column_name>* pair.

.. code-block:: bash

   $ $ydb_dist/yottadb -run %XCMD 'KILL ^%ydboctoocto("xref_status","*<table_name>*","*<column_name>*")'

The above command invalidates the current cross reference.

Run your original query, in Octo, to rebuild/fix the cross reference.
