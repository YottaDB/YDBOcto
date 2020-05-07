#################################################################
#                                                               #
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #   
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- select with limit having numeric values
select * from names limit 1.3;
select * from names limit 1.6;
-- -- below 3 queries check if limit clause still works after fraction usage
select * from names limit 2;
select * from names limit 2.3;
select * from names limit 2.6;
select * from names limit 3.449;
select * from names limit 3.450;
-- -- edge cases
select * from names limit .3; 
select * from names limit .5; 
select * from names limit 2.; 

