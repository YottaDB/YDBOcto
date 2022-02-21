#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBT09 : OCTO498 : Confirm proper number of plans generated for queries using IS (NOT) TRUE, IS (NOT) FALSE, and IS (NOT) UNKNOWN" {

select * from stock_availability where available is false;
select * from stock_availability where available is true;
select * from stock_availability where available is null;
select * from stock_availability where available is unknown;
select * from stock_availability where available is not false;
select * from stock_availability where available is not true;
select * from stock_availability where available is not null;
select * from stock_availability where available is not unknown;
select * from stock_availability order by available is false, product_id;
select * from stock_availability order by available is not false, product_id;
select * from stock_availability order by available is true, product_id;
select * from stock_availability order by available is not true, product_id;
select * from stock_availability order by available is null, product_id;
select * from stock_availability order by available is not null, product_id;
select * from stock_availability order by available is unknown, product_id;
select * from stock_availability order by available is not unknown, product_id;
