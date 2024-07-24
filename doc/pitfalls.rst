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

  To accelerate query execution, Octo automatically builds and maintains cross references (metadata). In the unlikely event that the data and metadata get out of sync (e.g., as a consequence of operational issues), run the below command to delete metadata. You can also delete metadata to shrink a database to reduce the amount of data to be copied, at the cost of having to recreate the metadata when queries are run subsequently.

  To delete all cross references (i.e. for all table and columns) run the following

  .. code-block:: bash

     $ OCTO> DISCARD XREFS;

  To delete cross references for a particular table for example :code:`names` run the following

  .. code-block:: bash

     $ OCTO> DISCARD XREFS names;

  After cross reference is deleted run your original query, in Octo, to rebuild the cross reference.
