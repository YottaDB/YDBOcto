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
# https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/737
discard all;
select count(*) from ORDER1 where ORDER1 IS NULL;
select count(*) from ORDER1 where OBJECT_OF_ORDER IS NULL;
select count(*) from ORDER1 where CURRENT_AGENT_PROVIDER IS NULL;
select count(*) from ORDER1 where DIALOG IS NULL;
select count(*) from ORDER1 where WHO_ENTERED IS NULL;
select count(*) from ORDER1 where WHEN_ENTERED IS NULL;
select count(*) from ORDER1 where STATUS IS NULL;
select count(*) from ORDER1 where PATIENT_LOCATION IS NULL;
select count(*) from ORDER1 where ITEM_ORDERED IS NULL;
select count(*) from ORDER1 where VEILED IS NULL;
select count(*) from ORDER1 where TYPE IS NULL;
select count(*) from ORDER1 where REPLACED_ORDER IS NULL;
select count(*) from ORDER1 where REPLACEMENT_ORDER IS NULL;
select count(*) from ORDER1 where PATIENT_CLASS IS NULL;
select count(*) from ORDER1 where TREATING_SPECIALTY IS NULL;
select count(*) from ORDER1 where PACKAGE IS NULL;
select count(*) from ORDER1 where COST IS NULL;
select count(*) from ORDER1 where SIGNATURE_REQUIRED IS NULL;
select count(*) from ORDER1 where EVENT IS NULL;
select count(*) from ORDER1 where PATIENT_APPOINTMENT IS NULL;
select count(*) from ORDER1 where START_DATE IS NULL;
select count(*) from ORDER1 where STOP_DATE IS NULL;
select count(*) from ORDER1 where `TO` IS NULL;
select count(*) from ORDER1 where CURRENT_ACTION IS NULL;
select count(*) from ORDER1 where DATE_OF_LAST_ACTIVITY IS NULL;
select count(*) from ORDER1 where GRACE_DAYS_BEFORE_PURGE IS NULL;
select count(*) from ORDER1 where ALERT_ON_RESULTS IS NULL;
select count(*) from ORDER1 where PARENT IS NULL;
select count(*) from ORDER1 where LAPSED_DATE_TIME IS NULL;
select count(*) from ORDER1 where SERVICE_CONN_CONDITION IS NULL;
select count(*) from ORDER1 where MST IS NULL;
select count(*) from ORDER1 where AGENT_ORANGE_EXPOSURE IS NULL;
select count(*) from ORDER1 where IONIZING_RAD_EXPOSURE IS NULL;
select count(*) from ORDER1 where SOUTHWEST_ASIA_CONDITIONS IS NULL;
select count(*) from ORDER1 where HEAD_AND_OR_NECK_CANCER IS NULL;
select count(*) from ORDER1 where COMBAT_VETERAN IS NULL;
select count(*) from ORDER1 where SHIPBOARD_HAZARD IS NULL;
select count(*) from ORDER1 where NATURE_OF_DC IS NULL;
select count(*) from ORDER1 where DC_ED_BY IS NULL;
select count(*) from ORDER1 where DC_DATE_TIME IS NULL;
select count(*) from ORDER1 where DC_REASON IS NULL;
select count(*) from ORDER1 where DC_REASON_TEXT IS NULL;
select count(*) from ORDER1 where COMPLETED IS NULL;
select count(*) from ORDER1 where COMPLETED_BY IS NULL;
select count(*) from ORDER1 where DC_EVENT IS NULL;
select count(*) from ORDER1 where DC_ORIGINAL_ORDER IS NULL;
select count(*) from ORDER1 where RESULTS_DATE_TIME IS NULL;
select count(*) from ORDER1 where ABNORMAL_RESULTS IS NULL;
select count(*) from ORDER1 where FINDINGS IS NULL;
select count(*) from ORDER1 where BA_SERVICE_CONN_CONDITION IS NULL;
select count(*) from ORDER1 where BA_MILITARY_SEXUAL_TRAUMA IS NULL;
select count(*) from ORDER1 where BA_AGENT_ORANGE_EXPOSURE IS NULL;
select count(*) from ORDER1 where BA_IONIZING_RAD_EXPOSURE IS NULL;
select count(*) from ORDER1 where BA_SOUT_ASIA_CONDITIONS IS NULL;
select count(*) from ORDER1 where BA_HEAD_AND_OR_NECK_CANCER IS NULL;
select count(*) from ORDER1 where BA_COMBAT_VETERAN IS NULL;
select count(*) from ORDER1 where PFSS_ACCOUNT_REFERENCE IS NULL;
select count(*) from ORDER1 where BA_SHIPBOARD_HAZARD IS NULL;
