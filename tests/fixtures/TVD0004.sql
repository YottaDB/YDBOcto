#################################################################
#								#
# Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	#
# Copyright (c) 2021 Chris Combs
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TVD0004 : Test Realistic Sample Query 2

SELECT A.ORDER1_ID,
E.NAME,
A.CURRENT_ACTION,
F.DISPLAY_GROUP_ID,
F.NAME as Order_Category,
G.ORDER_ORDER_ACT_ORDER_TEXT_ID as ORDER_TEXT_ID,
G.ORDER_TEXT
FROM ORDER1 A
LEFT JOIN PATIENT E ON NUMBER(A.OBJECT_OF_ORDER)=E.PATIENT_ID
LEFT JOIN ORDER_STATUS C ON (A.STATUS=C.ORDER_STATUS_ID)
LEFT JOIN ORDER_ORDER_ACTIONS B ON
(
   A.ORDER1_ID = B.ORDER1_ID
   AND A.CURRENT_ACTION=B.ORDER_ORDER_ACTIONS_ID
)
LEFT join ORDER_ORDER_ACT_ORDER_TEXT G on (A.ORDER1_ID= G.ORDER1_ID and B.ORDER_ORDER_ACTIONS_ID=G.ORDER_ORDER_ACTIONS_ID)
LEFT JOIN NEW_PERSON D ON (B.SIGNED_BY = D.NEW_PERSON_ID)
LEFT JOIN DISPLAY_GROUP F ON (A.TO=F.DISPLAY_GROUP_ID)
WHERE
   A.CURRENT_ACTION is not null
   AND A.STATUS in (3,4,5,6,8,9,11,15)
   AND (A.STOP_DATE>=now() OR A.STOP_DATE IS NULL)
   AND (A.OBJECT_OF_ORDER is not null and E.CURRENT_MOVEMENT is not null AND E.NAME NOT LIKE 'ZZ%')
   AND C.NAME='ACTIVE'
order by e.patient_id,a.order1_id,G.ORDER_ORDER_ACT_ORDER_TEXT_ID

