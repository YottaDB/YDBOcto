#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR025 : OCTO483 : Issue error when invalid integer values are used in CREATE TABLE DELIM list

-- No negative integers
CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM (-1) GLOBAL "^delimnames(keys(""id""))";

-- No values greater than UTF-8 max code point value (i.e. MAX_UTF8_VALUE = 1112064)
CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM (1112065) GLOBAL "^delimnames(keys(""id""))";
CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM (111206500) GLOBAL "^delimnames(keys(""id""))";
