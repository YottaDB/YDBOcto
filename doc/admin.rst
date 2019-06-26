
========================
Administration
========================

Octo uses a tool to manage the users of the database and other administrative tasks, called ydboctoAdmin.

----------------
ydboctoAdmin
----------------

ydboctoAdmin allows the user to add, delete and view users.

Usage pattern:

.. parsed-literal::
   ydboctoAdmin <action> <subAction> <arguments>

* <action> is either add, delete or view.
* <subAction> refers to :code:`user` or :code:`users`.
* <arguments> would be the specific arguments passed to the command.

+++++++++++++
Add
+++++++++++++

The following is an example of adding users to the database:

.. parsed-literal::
   ydbOctoAdmin add user OctoUser

This adds OctoUser as a user of the database, after verifying the password for the user.

++++++++++++++
Delete
++++++++++++++

The following is an example of deleting users from the database:

.. parsed-literal::
   ydboctoAdmin delete user OctoUser

This deletes OctoUser from the list of users in the database.

++++++++++++++
View
++++++++++++++

The following is an example of viewing users in the database:

.. parsed-literal::
   ydboctoAdmin view users

This shows a list of the users of the database.
