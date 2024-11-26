#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TVD0013 : OCTO1069 : Test Outer joins with Computed Fields
SELECT pm.ward_at_discharge IS NULL as is_null, count(pm.ward_at_discharge IS NULL)
FROM patient p
LEFT JOIN patient_movement pm on p.patient_id = pm.patient
GROUP BY pm.ward_at_discharge IS NULL;
