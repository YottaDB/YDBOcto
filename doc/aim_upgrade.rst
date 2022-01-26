.. #################################################################
.. #								   #
.. # Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.       #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

==============================================================
Octo 1.1 (Octo with AIM) Upgrade for Existing Octo VistA Users
==============================================================

.. contents::
   :depth: 2

------------
Introduction
------------
Effective r1.10.0, Octo uses the `Application Independent Metadata
<https://docs.yottadb.com/Plugins/ydbaim.html>`_ plugin to maintain metadata
(cross references and statistics) for Octo. Previously, Octo used its own code
for metadata. AIM brings several advantages to Octo:

* If the parent record of a VistA record is removed, AIM removes the metadata.
  Previously, with the internally maintained metadata, removal of the parent
  record would not remove the metadata, resulting in an out of design
  condition. [https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/630]
* AIM allows triggers to be specified on ranges or subsets of subscripts at
  each level. Octo previously cross referenced all subscripts at each level.
  Since Fileman only uses a subset of possible subscripts at each level, this
  meant that if application code set global nodes with subscripts outside a
  range or subset used by Fileman, those nodes would be cross referenced by
  Octo, potentially resulting in incorrect query results.

AIM requires YottaDB r1.34 or later, as well as some steps to migrate from 1.0
versions of Octo. This document guides you through the migration.

The VistA DDL with AIM requires that your database files disallow NULL
subscripts.  Normally, VistA database files do not have NULL subscripts
enabled; but we mention this here for completeness' sake.

--------------------------------
Upgrade YottaDB and Install Octo
--------------------------------
You can install the latest YottaDB and Octo in a single step with :code:`sudo
ydbindstall.sh --octo` or by executing :code:`ydbinstall.sh --octo` as root. If
you already have YottaDB r1.34 or later installed, :code:`sudo ydbinstall.sh
--octo --plugins-only` will install Octo, or update Octo if the latest version
is not installed. Installing Octo with :code:`ydbinstall` /
:code:`ydbinstall.sh` also installs plugins that Octo depends on.

.. code-block:: bash

   mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
   cd /tmp/tmp ; chmod +x ydbinstall.sh
   sudo ./ydbinstall.sh --verbose --octo

---------------------------------------------------
Stop VistA processes and :code:`octo`/:code:`rocto`
---------------------------------------------------
If you are running Octo directly on the VistA server (which we recommend
against), you will need to stop all VistA processes, in addition to stopping
octo/rocto. If you are using Octo on a replicated server, you do not need to
stop any VistA processes as none are running. If you set-up system services
(e.g. rocto as a system service), then you need to stop the system services
(this part is not shown below).

.. code-block:: bash

   $ydb_dist/mumps -r HALTALL^ZSY
   pkill octo
   pkill rocto

-----------------------------------
Run down Databases and Relink Files
-----------------------------------
This is strictly not needed, but is a safety check to ensure that there are no
processes currently connected to the database.

.. code-block:: bash

   $ydb_dist/mupip rundown -r '*'
   $ydb_dist/mupip rundown -relinkctl

-------------------------------
Stop journaling and replication
-------------------------------
Replication not shown.

.. code-block:: bash

   $ydb_dist/mupip set -journal="off" -reg '*'

----------------------------------------------------------
Adjust VistA environment to use the new version of YottaDB
----------------------------------------------------------
Adjust your environment file to update $ydb_dist. After you make that change,
go back into YottaDB to check that you upgraded successfully. By default,
YottaDB is installed in :code:`/usr/local/lib/yottadb/<version>/`. You should
use that value. After you update your environment file and re-source it, you
should see the following when you go back to YottaDB. Make sure it says YottaDB
r1.34 or higher.

.. code-block:: bash

    $ydb_dist/mumps -dir

    VEHU>w $zv
    GT.M V6.3-011 Linux x86_64
    VEHU>w $zyre
    YottaDB r1.34 Linux x86_64

--------------------------------------------------
Add :code:`ydb_env_set` to your environment set-up
--------------------------------------------------
Adding :code:`ydb_env_set` to your environment file will automatically upgrade your global directory, and add all Plugins to :code:`gtmroutines`. First, edit `gtmroutines` to remove all plugins, and then add :code:`ydb_env_set` after it. For example:

.. code-block:: bash

   export gtmroutines="/home/vehu/p/r1.34_x86_64*(/home/vehu/p) /home/vehu/s/r1.34_x86_64*(/home/vehu/s) /home/vehu/r/r1.34_x86_64*(/home/vehu/r)"
   source $gtm_dist/ydb_env_set

If you have object directories that need to be created, please do so now. For example:

.. code-block:: bash

   mkdir -p /home/vehu/{p,s,r}/r1.34_x86_64

After you update your environment file re-source it. Confirm the new :code:`gtmroutines`:

.. code-block:: bash

   echo $gtmroutines
   /home/vehu/p/r1.34_x86_64*(/home/vehu/p) /home/vehu/s/r1.34_x86_64*(/home/vehu/s) /home/vehu/r/r1.34_x86_64*(/home/vehu/r) /usr/local/lib/yottadb/r134/plugin/o/_ydbaim.so /usr/local/lib/yottadb/r134/plugin/o/_ydbocto.so /usr/local/lib/yottadb/r134/plugin/o/_ydbposix.so /usr/local/lib/yottadb/r134/plugin/o/libcocto.so /usr/local/lib/yottadb/r134/libyottadbutil.so

Confirm that the routines and globals work correctly by running :code:`SILENT^%RSEL` and :code:`^%GD`.

.. code-block:: bash

   $ydb_dist/mumps -r %XCMD 'D SILENT^%RSEL("*")'
   $ydb_dist/mumps -r %GD <<< ""

The output of the first command should be empty; the second command will print a list of globals.

--------------------------------
Recompile Object code (optional)
--------------------------------
This is optional, as YottaDB will recompile the code on demand. Note that if
you need to create directories because of your particular set-up for different
versions of objects, you must do that in advance.
The following steps vary depending on your :code:`$gtmroutines` set-up. Here's an
example:

.. code-block:: bash

    # Compile the objects in each directory
    cd /home/vehu/p/r1.34_x86_64/
    find .. -name '*.m' | xargs --max-procs=$(getconf _NPROCESSORS_ONLN) --max-args=1 $ydb_dist/mumps
    cd /home/vehu/s/r1.34_x86_64/
    find .. -name '*.m' | xargs --max-procs=$(getconf _NPROCESSORS_ONLN) --max-args=1 $ydb_dist/mumps
    cd /home/vehu/r/r1.34_x86_64/
    find .. -name '*.m' | xargs --max-procs=$(getconf _NPROCESSORS_ONLN) --max-args=1 $ydb_dist/mumps
    cd

If you have an o and r/p directories, you can adapt the instructions above to look like this

.. code-block:: bash

    cd /home/vehu/o
    rm *.o
    find ../r/ -name '*.m' xargs --max-procs=$(getconf _NPROCESSORS_ONLN) --max-args=1 $ydb_dist/mumps
    find ../p/ -name '*.m' xargs --max-procs=$(getconf _NPROCESSORS_ONLN) --max-args=1 $ydb_dist/mumps
    cd

-------------------------
Update the Octo VistA DDL
-------------------------
TODO: After YDBOctoVistA#24 code is merged, update URLs below to point to master.

**This is an important step**. Make sure you use the _YDBOCTOVISTAM.m v1.4 or
later. Not doing so will make "IS NULL" queries potentially give you incorrect
data.

Remember that earlier :code:`$gtmroutines` directories override later ones.
Make sure you put the new copy of `_YDBOCTOVISTAM.m` over the first one, or
ensure that there are no other copies.

The following commands locate the earlier copy of `_YDBOCTOVISTAM.m` and
replace it with the new code:

.. code-block:: bash

    $ find . -name '_YDBOCTOVISTAM.m'
    ./p/_YDBOCTOVISTAM.m
    $ curl -L https://gitlab.com/shabiel/YDBOctoVistA/-/raw/ddl24-aimmetadatatype/_YDBOCTOVISTAM.m -o ./p/_YDBOCTOVISTAM.m

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

--------------------------------------
Restart journaling and VistA processes
--------------------------------------
After this, you can start the processes again, in the following order:

1. Enable Journaling (e.g. :code:`$ydb_dist/mupip set -journal=on -reg '*'`)
2. Enable replication (not shown)
3. Enable Services (not shown)
4. If you shutdown VistA, start Taskman, which automatically starts the other
   processes (e.g. :code:`$ydb_dist/mumps -r ZTMB`)

At this point, you are done with the Octo 1.1/Octo AIM upgrade.
