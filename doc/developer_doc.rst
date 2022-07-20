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

==========================
Developer's Documentation
==========================

.. contents::
   :depth: 3

-------------
Contributing
-------------

To contribute or help with further development, `fork <https://docs.gitlab.com/ee/gitlab-basics/fork-project.html>`_ the `YDBOcto repository <https://gitlab.com/YottaDB/DBMS/YDBOcto>`_, clone your fork to a local copy and begin contributing!

Please also set up the pre-commit script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

.. code-block:: bash

   ln -s ../../pre-commit .git/hooks

Note that this script will require :code:`tcsh` and :code:`clang-format-11` or a later release.

.. code-block:: bash

   # Ubuntu 20.04
   sudo apt install --no-install-recommends clang-format-11
   # Any Debian-like distro; see also https://apt.llvm.org/
   bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
   # CentOS 8
   sudo yum install clang-tools-extra

+++++++++++
clang-tidy
+++++++++++

The CI pipeline will run the `clang-tidy <https://clang.llvm.org/extra/clang-tidy/>`_ tool to catch common errors. You can replicate its behavior locally as follows:

.. code-block:: bash

   # Ubuntu 20.04
   sudo apt install --no-install-recommends clang-tidy
   # Any Debian-like distro
   bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
   # CentOS 8
   sudo yum install clang-tools-extra

   mkdir build
   cd build
   cmake -D CMAKE_EXPORT_COMPILE_COMMANDS=ON ..
   clang-tidy ../src/octo_init.c  # replace octo_init.c with the file you want to check

:code:`clang-tidy-8` and later are supported.

+++++++++
YDBCMake
+++++++++

Octo uses the upstream `YDBCMake <https://gitlab.com/YottaDB/Tools/YDBCMake>`_ repository to build using YottaDB as the M compiler. Any changes to :code:`ydbcmake/` should first be upstreamed to that repository.

Once the changes are upstreamed, you can merge them into Octo using:

.. code-block:: bash

   git pull --no-rebase git@gitlab.com:YottaDB/Tools/YDBCMake.git

+++++++++++
Dockerfiles
+++++++++++
There are 4 Dockerfiles at the top of the source tree:

- :code:`Dockerfile`
- :code:`Dockerfile-Tests.rocky`
- :code:`Dockerfile-Tests.ubuntu`
- :code:`Dockerfile-Tests.vista`

:code:`Dockerfile` builds a docker container suitable for use for using Octo in
a testing capacity. The other files are all testing related, and are used to
replicate the Gitlab pipelines. There are instructions at the top of each file
for usage as well as current limitations.
