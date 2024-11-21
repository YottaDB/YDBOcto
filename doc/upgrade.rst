.. #################################################################
.. #								   #
.. # Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.       #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

==================
Octo Upgrade Guide
==================

.. contents::
   :depth: 2

------------
Introduction
------------

This guide describes the process for upgrading Octo from an older to a newer version. Before you start, make sure you source your environment file so that you are ready to execute YottaDB commands.

-----------------------------
Get your current Octo version
-----------------------------

Run ``octo --version`` to get the current version that you will be upgrading and note it down. This will help you troubleshoot any upgrade issues if you run into trouble.

-------------------
Stop Octo processes
-------------------

If you set-up system services (e.g. rocto as a system service), then you need to stop the system services (this part is not shown below).

  .. code-block:: bash

     pkill octo
     pkill rocto

-----------------------------------
Run down Databases and Relink Files
-----------------------------------

This is strictly not needed, but is a safety check to ensure that there are no processes currently connected to the database.

  .. code-block:: bash

     $ydb_dist/mupip rundown -r '*'
     $ydb_dist/mupip rundown -relinkctl

-------------------------------
Stop journaling and replication
-------------------------------

Replication not shown.

  .. code-block:: bash

     $ydb_dist/mupip set -journal="off" -reg '*'

-------------------
Install Latest Octo
-------------------

You can install the latest YottaDB and Octo in a single step with :code:`sudo ydbintstall.sh --octo` or by executing :code:`ydbinstall.sh --octo` as root. If you already have YottaDB r1.34 or later installed, :code:`sudo ydbinstall.sh --octo --plugins-only --overwrite-existing` will install Octo, or update Octo if the latest version is not installed. Installing Octo with :code:`ydbinstall` /:code:`ydbinstall.sh` also installs plugins that Octo depends on.

  .. code-block:: bash

     mkdir /tmp/tmp ; wget -P /tmp/tmp https://download.yottadb.com/ydbinstall.sh
     cd /tmp/tmp ; chmod +x ydbinstall.sh
     sudo ./ydbinstall.sh --octo --plugins-only --overwrite-existing

-------------------------
Update the Octo VistA DDL
-------------------------

If you use Octo with VistA, you should also update the "VistA DDL" if it has changed since your last Octo.

Remember that earlier ``$gtmroutines``/``$ydb_routines`` directories override later ones. Make sure you put the new copy of ``_YDBOCTOVISTAM.m`` over the first one, or ensure that there are no other copies.


The following example illustrates determining the location of ``_YDBOCTOVISTAM.m`` and replacing it with the latest code:

  .. code-block:: bash

      $ find . -name '_YDBOCTOVISTAM.m'
      ./p/_YDBOCTOVISTAM.m
      $ curl -L https://gitlab.com/YottaDB/DBMS/YDBOctoVistA/-/raw/master/_YDBOCTOVISTAM.m -o ./p/_YDBOCTOVISTAM.m

Now run the VistA DDL Generator to produce the VistA DDL:

  .. code-block:: bash

      $ $ydb_dist/yottadb -r %XCMD 'S DUZ=.5,DIQUIET=1,DUZ(0)="@" D DT^DICRW,MAPALL^%YDBOCTOVISTAM("vista.sql")'

Load the generated DDL into Octo:

  .. code-block:: bash

      $ $ydb_dist/plugin/bin/octo -f vista.sql

Verify functionality by running a sample query

  .. code-block:: bash

      $ curl -LO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/TVD0006.sql
      $ $ydb_dist/plugin/bin/octo -f TVD0006.sql

If you see results all is well. You can delete :code:`TVD0006.sql` file after that.

------------------
Restart journaling
------------------

After this, you can start the processes again, in the following order:

1. Enable Journaling (e.g. :code:`$ydb_dist/mupip set -journal=on -reg '*'`)
2. Enable replication (not shown)
3. Enable Services (not shown)

At this point, you are done with the Octo upgrade.
