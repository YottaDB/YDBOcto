#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDFT08 : OCTO54 : Test that DELETE FROM correctly deletes duplicate rows on table with no primary key columns

drop table if exists TDFT08;
create table TDFT08 (id INTEGER, firstname VARCHAR);
insert into TDFT08 values (1234, 'First'), (1234, 'First');
delete from TDFT08 where firstname = 'First';
select * from TDFT08;
insert into TDFT08 values (1234, 'First'), (1234, 'First');
delete from TDFT08 where id = 1234;
select * from TDFT08;

