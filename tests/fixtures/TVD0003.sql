#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# Copyright (c) 2021 Chris Combs
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# TVA0003 : Test Realistic Sample Query 1
SELECT P.NAME AS PATIENT_NAME, P.PATIENT_ID as PATIENT_ID,
       P.WARD_LOCATION,
       TOKEN(REPLACE(TOKEN(REPLACE(P.WARD_LOCATION,"WARD ",""),"-",1),"WARD ","")," ",2) AS PCU,
       CONCAT(TOKEN(REPLACE(TOKEN(REPLACE(P.WARD_LOCATION,"WARD ",""),"-",2),"WARD ","")," ",1)," ",TOKEN(P.WARD_LOCATION,"-",3)) AS UNIT,
       P.ROOM_BED as ROOM_BED,
       REPLACE(P.DIVISION,"VEHU","") as FACILTY,
       P.SEX as SEX,
       P.CURRENT_ADMISSION as CURRENT_ADMISSION,
       P.CURRENT_MOVEMENT as CURRENT_MOVEMENT,
       DATEFORMAT(P.DATE_OF_BIRTH,"5Z") as DATE_OF_BIRTH,
       -- P.Age, -- Don't add Age to pipeline as it will keep changing
       PM.PATIENT_MOVEMENT_ID as Current_Patient_Movement,
       PM.TYPE_OF_MOVEMENT as Current_Movement_Type,
       AM.PATIENT_MOVEMENT_ID as Admission_Movement,
       AM.TYPE_OF_MOVEMENT as Admission_Type
FROM PATIENT P
left join patient_movement PM on P.CURRENT_MOVEMENT=PM.PATIENT_MOVEMENT_ID
left join patient_movement AM on P.CURRENT_ADMISSION=AM.PATIENT_MOVEMENT_ID
where P.CURRENT_MOVEMENT is not null
and P.ward_location not like "ZZ%" and P.NAME not like "ZZ%";
