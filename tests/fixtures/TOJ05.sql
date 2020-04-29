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

-- TOJ05 : OCTO312 : Use WHERE clause to optimize LEFT JOIN (just like INNER JOIN)
CREATE FUNCTION CURR_TIMESTAMP(VARCHAR) RETURNS NUMERIC AS $$^TIMESTAMP;

SELECT A.ORDER1_ID, A.PATIENT_ID, A.OBJECT_OF_ORDER
FROM ORDER1 A
LEFT JOIN PATIENT E ON (A.PATIENT_ID=E.PATIENT_ID)
LEFT JOIN ORDER_STATUS C ON (A.STATUS=C.ORDER_STATUS_ID)
LEFT JOIN ORDER_ORDER_ACTIONS B ON ( A.ORDER1_ID = B.ORDER1_ID AND A.CURRENT_ACTION=B.ORDER_ORDER_ACTIONS_ID)
LEFT JOIN NEW_PERSON D ON (B.SIGNED_BY = D.NEW_PERSON_ID)
LEFT JOIN DISPLAY_GROUP F ON (A.TO=F.DISPLAY_GROUP_ID)
WHERE
(
   A.PATIENT_ID>0
   and A.CURRENT_ACTION>0
   AND A.STATUS in (5,6,15)
   AND ( A.STOP_DATE>=CURR_TIMESTAMP("V")::NUMERIC OR A.STOP_DATE IS NULL )
   AND E.NAME NOT LIKE "ZZ%"
   AND E.CURRENT_MOVEMENT>0
);

