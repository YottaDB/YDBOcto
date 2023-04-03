#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

select v1.firstname,n1.firstname from v1 as n1; -- ERROR
-- names=> select v1.firstname,n1.firstname from v1 as n1;
--ERROR:  42P01: relation "v1" does not exist
--LINE 1: select v1.firstname,n1.firstname from v1 as n1;
--                                              ^
--LOCATION:  parserOpenTable, parse_relation.c:1180

select v1.n1.id,v1.n1.firstname,v1.n1.lastname from v1; -- ERROR:  missing FROM-clause entry for table "n1"
-- names=> select v1.n1.id,v1.n1.firstname,v1.n1.lastname from v1;
-- ERROR:  42P01: relation "v1" does not exist
-- LINE 1: select v1.n1.id,v1.n1.firstname,v1.n1.lastname from v1;
--                                                            ^
--LOCATION:  parserOpenTable, parse_relation.c:1180

