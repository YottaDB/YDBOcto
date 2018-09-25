
==================================
Tips and Tricks
==================================

.. contents::
   :depth: 5

----------------------------------
Creating N-key CURSOR Statements
----------------------------------

If you ae mapping an existing MUMPS schema, you may need to generate a CURSOR statement for multiple keys.

.. note::
   It may be beneficial to refresh your memory with the definitions of `$ORDER <https://docs.yottadb.com/ProgrammersGuide/functions.html#order>`_ and `$SELECT <https://docs.yottadb.com/ProgrammersGuide/functions.html#select>`_ and how they work.

For the sake of clarity, the extra quotes needed to escape these in SQL are omitted.

+++++++++++
For 1 key
+++++++++++

.. parsed-literal::
   SET keys(0)=$ORDER(^global(keys(0)))

The above command advances the cursor by one.

**Example and Explanation**:

.. parsed-literal::
   YDB> zwrite ^Animals
   ^Animals("Dog")=""
   ^Animals("Cat")=""

Here, keys(0) is initially at "Dog" and `$ORDER <https://docs.yottadb.com/ProgrammersGuide/functions.html#order>`_ advances the cursor to point to "Cat".

++++++++++++
For 2 keys
++++++++++++

.. parsed-literal::
   SET k0=keys(0)                                                                              # Note down keys(0)
   SET keys(1)=$SELECT(""=keys(0):"",1:$ORDER(^global(keys(0),keys(1)))                        # Advance keys(1)
   SET keys(0)=$SELECT(""=keys(1):$ORDER(^global(keys(0))),1:keys(0))                          # Check if keys(1) is NULL, if so, advance keys(0)
   SET keys(1)=$SELECT(""=keys(0):"",keys(0)'=k0:$ORDER(^global(keys(0),"")),1:keys(1))        # If keys(0) changed, restart keys(1), else, leave keys(1)

The above command advances the cursor by one when there are two keys.

**Example and Explanation**:

.. parsed-literal::
   YDB>zwrite ^LastName
   ^LastName("Doe",2)=""
   ^LastName("Smith",1)=""
   ^LastName("Greene",0)=""
   ^LastName("Brown",3)=""

In the above example, keys(0)= "Doe" and keys(1)= 2. We want to use the cursor to advance, so that keys(0) becomes "Smith" and keys(1) becomes 1.

.. parsed-literal::
   set k0=keys(0)

k0 ="Doe"

.. parsed-literal::
   SET keys(1)=$SELECT(""=keys(0):"",1:$ORDER(^global(keys(0),keys(1)))

keys(0)="Doe", keys(1)=2

Since keys(0) is not "", we run $ORDER on keys(1).

.. note::
    The purpose of checking keys(0) for "" is to safeguard against a `NULSUBSC <https://docs.yottadb.com/MessageRecovery/errors.html#nulsubsc>`_ error.

keys(1) is "" because if $ORDER finds no node (or name) at the specified level after the specified variable, it returns an null string (”“).

.. parsed-literal::
   SET keys(0)=$SELECT(""=keys(1):$ORDER(^global(keys(0))),1:keys(0))

keys(0)="Doe", keys(1)=""

keys(1) is "" now, so we run $ORDER on keys(0).

keys(0) is now "Smith".

.. parsed-literal::
   SET keys(1)=$SELECT(""=keys(0):"",keys(0)'=k0:$ORDER(^global(keys(0),"")),1:keys(1))

keys(0)="Smith", keys(1)=""

Since keys(0) is not "", we move to the next part.

keys(0) is now "Smith", and k0 is still "Doe", so we see that keys(0) has changed from its initial value.

Because of that, $ORDER will now be applied and keys(1) becomes 1.

Now that the CURSOR has been run, we can see that it advanced from ^LastName("Doe",2) to ^LastName("Smith",1).

+++++++++++++
For 3 keys
+++++++++++++

.. parsed-literal::
   SET k0=keys(0)k1=keys(1)                                                                                                      # Note down keys(0) and keys(1)
   SET keys(2)=$SELECT(""=keys(0):"","=keys(1):"",1:$ORDER(^global(keys(0),keys(1),keys(2)))                                     # Advance keys(2)
   SET keys(1)=$SELECT(""=keys(0):"",""=keys(2):$ORDER(^global(keys(0),keys(1)),1:keys(1)                                        # Advance keys(1)
   SET keys(0)=$SELECT(""=keys(1):$ORDER(^global(keys(0))),1:keys(0))                                                            # Check if keys(1) is NULL, if so, advance keys(0)
   SET keys(1)=$SELECT(""=keys(0):"",keys(0)'=k0:$ORDER(^global(keys(0),"")),1:keys(1))                                          # If keys(0) changed, restart keys(1), else, leave keys(1)
   SET keys(2)=$SELECT(""=keys(1):"",keys(1)'=k1:$ORDER(^global(keys(0),keys(1),"")),1:keys(2))                                  # If keys(1) changed, restart keys(2), else, leave keys(2)

Here, keys(0), keys(1) and keys(2) are the three keys.

**Example and Explanation**:

.. parsed-literal::
   YDB>zwrite ^FullNames
   ^FullNames("Doe","Jane",2)=""
   ^FullNames("Smith","John",1)=""
   ^FullNames("Greene","Edward",0)=""
   ^Full Names("Brown","James",3)=""

Initially, 

keys(0)="Doe", keys(1)="Jane",keys(2)=2

.. parsed-literal::
   SET k0=keys(0)k1=keys(1)

k0="Doe", k1="Jane"

.. parsed-literal::
   SET keys(2)=$SELECT(""=keys(0):"","=keys(1):"",1:$ORDER(^global(keys(0),keys(1),keys(2)))

keys(0) and keys(1) are not "", so we perform a $ORDER operation on keys(2).

keys(0)="Doe", keys(1)="Jane", keys(2)=""

.. parsed-literal::
   SET keys(1)=$SELECT(""=keys(0):"",""=keys(2):$ORDER(^global(keys(0),keys(1)),1:keys(1)

keys(0) is not "", but keys(2) is "", so we perform a $ORDER operation on keys(1).

keys(0)="Doe", keys(1)="", keys(2)=""

.. parsed-literal::
   SET keys(0)=$SELECT(""=keys(1):$ORDER(^global(keys(0))),1:keys(0))

keys(1) is "" , so we perform the $ORDER operation on keys(0).

keys(0)="Smith", keys(1)="", keys(2)=""

.. parsed-literal::
   SET keys(1)=$SELECT(""=keys(0):"",keys(0)'=k0:$ORDER(^global(keys(0),"")),1:keys(1))

keys(0) is not "", so we move to the second part.

keys(0)="Smith", which does not equal k0="Doe", so we perform a $ORDER operation on keys(1).

keys(0)="Smith", keys(1)="John", keys(2)=""

.. parsed-literal::
   SET keys(2)=$SELECT(""=keys(1):"",keys(1)'=k1:$ORDER(^global(keys(0),keys(1),"")),1:keys(2))

keys(1) is not "", so we move to the second part.

keys(1)="John" which is not equal to k1="Jane", so we perform a $ORDER operation on keys(2).

keys(0)="Smith", keys(1)="John", keys(2)=1

And hence the CURSOR has advanced.

++++++++++++
For N keys
++++++++++++

.. parsed-literal::
   SET ... # note down keys(0) .. keys(N-1)
   SET ... # Advance keys(N)
   SET ... # Advance keys (N - 1)
   SET ... # Advance keys (N - 2)
   ...
   SET ... # Advance keys(0)
   SET ... # If keys(0) changed, restart keys(1)
   SET ... # If keys(1) changed, restart keys(2)
   ...
   SET ... # If keys(N - 1) changed, restart keys(N)

The above set of statements can be adapted to create CURSOR commands for any number of keys.

The END statement will always be when keys(0)="".

.. note::
   The CURSOR creation templates on this page have been broken into multiple SET lines for clarity; in practice, all the lines need to be one command.
