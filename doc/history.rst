.. #################################################################
.. #								   #
.. # Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.       #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

====================
History
====================

.. contents::
   :depth: 2

------------
Introduction
------------

Octo uses the `readline library
<https://tiswww.case.edu/php/chet/readline/rltop.html>`_ to provide
editing history and searching. This document provides a few pointers on
how to use readline with Octo, but the user is best served by reading the
readline documentation for finer points.

------------
History File
------------

Octo reads history when started; and saves it when shutting
down. The history file is by default in :code:`~/.octo_history`, but can be
overridden to a different location by using the Octo :doc:`config` file
:code:`octo_history` setting.  The reference config file in
:code:`$ydb_dist/plugin/octo/octo.conf` stores the history in
:code:`~/.octo_history`, which is the same as the default if a config file does
not exist.

--------------
History Length
--------------
History length by default is 500. It can be overridden using the
:code:`octo_history_max_length` setting in using the Octo :doc:`config` file.
The reference config file in :code:`$ydb_dist/plugin/octo/octo.conf` sets this
to 500, which is the same as the default if a config file does not exist.

This history length applies only when octo saves the history back to the file
at process exit time; irrespective of this length, octo will still read as much
history as available in the history file pointed to by the `octo_history`
config setting. And also irrespective of this length, while Octo is running,
there is no history trimming being performed, so you can have more history
while running Octo than the limit specified here.

When saving is done at exit time, history will be trimmed to
`octo_history_max_length`.

If you set `octo_history_max_length` to zero, it disables saving history. If
you set `octo_history_max_length` to less than zero, it will be clamped down
to zero.

------------------------
Usual History Operations
------------------------
The the usual operations to perform with history are as follows:

* Use the :code:`up arrow` and :code:`down arrow` to navigate up and down your
  history list.
* Use :code:`CTRL-R` shortcut to reverse search history.
* Use :code:`\s<enter>` to view your history.
