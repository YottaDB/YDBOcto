#################################################################
#                                                               #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- TRTE09 : OCTO972 : Verify the structure of the regex expression after a regex to equals conversion
-- In this case, since the type is not easy to determine, binary_operation_data_type_check() will take care of
-- regex to equals optimization. One consequence of this is that if an expression like
-- `('test' || 'test') NOT LIKE 'test'` is used then before YDBOcto#972 this expression  would have been
-- converted to `('test' || 'test') != 'test'`, but after the change it will be converted to
-- `NOT(('test' || 'test') = 'test')`.
-- Optimization to the logical plan will convert `NOT(('test' || 'test') = 'test')` to `('test' || 'test') != 'test'`.
select * from names where (firstname || lastname) NOT LIKE 'test';
