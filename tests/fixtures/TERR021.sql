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

-- TERR021 : OCTO !630 : Check error return codes from ydb_ci_tab_open
-- Any query here should work, since octo will error immediately if the CI file doesn't exist.
SELECT * FROM names LIMIT 1;
