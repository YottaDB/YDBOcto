.. #################################################################
.. #								   #
.. # Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.  #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

========================
Administration
========================

.. contents::
   :depth: 4

Octo uses an M routine to manage the users of the database and other administrative tasks, called :code:`%ydboctoAdmin`.

Currently Octo user administration consists of user creation and deletion, and setting of user database permissions.

User database permissions are set at user-creation time, i.e. via :code:`%ydboctoAdmin add user`. If no options are passed to this command, the user is created with read-only permissions. To grant additional permissions, use :code:`--readwrite` or :code:`--allowschemachanges`, described below.

----------------
ydboctoAdmin
----------------

  %ydboctoAdmin allows the user to add, delete and view a list of users with show.

  Usage pattern:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin <action> <subAction> <arguments>

  * <action> is either add, delete or show.
  * <subAction> refers to :code:`user` or :code:`users`.
  * <arguments> would be the specific arguments passed to the command.

+++++++++++++
Add
+++++++++++++

  The following is an example of adding users to the database:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin add user OctoUser

  This adds OctoUser as a user of the database, after verifying the password for the user. Unless otherwise specified, new users are created with read-only database permissions.

  To grant a user read-write permissions (:code:`INSERT`, :code:`UPDATE`, and :code:`DELETE`) use the :code:`-w`/:code:`--readwrite` flag:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin add user OctoUser --readwrite
      yottadb -r %ydboctoAdmin add user OctoUser -w

  To grant a user read-write permissions and permission to modify schemas (:code:`CREATE` and :code:`DROP`), use the :code:`-a`/:code:`--allowschemachanges` flag:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin add user OctoUser --allowschemachanges
      yottadb -r %ydboctoAdmin add user OctoUser -a

++++++++++++++
Delete
++++++++++++++

  The following is an example of deleting users from the database:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin delete user OctoUser

  This deletes OctoUser from the list of users in the database.

++++++++++++++
Show
++++++++++++++

  The following is an example of the database showing a list of users:

   .. code-block:: bash

      yottadb -r %ydboctoAdmin show users

  This shows a list of the users of the database.
