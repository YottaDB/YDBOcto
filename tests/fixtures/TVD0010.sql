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
# TVD0010 : YDBOctoVistA#3 Break out Variable Pointer field into separate columns

-- Query that illustrates variable pointers broken out
select REACTANT, GMR_ALLERGY, GMR_ALLERGY_V_GMR_ALLERGIES, GMR_ALLERGY_V_VA_GENERIC, GMR_ALLERGY_V_DRUG, GMR_ALLERGY_V_DRUG_INGREDIENTS, GMR_ALLERGY_V_VA_DRUG_CLASS from PATIENT_ALLERGIES LIMIT 10;

-- Query that shows that IS NULL conditions work with these new fields
select REACTANT, GMR_ALLERGY, GMR_ALLERGY_V_GMR_ALLERGIES, GMR_ALLERGY_V_VA_GENERIC, GMR_ALLERGY_V_DRUG, GMR_ALLERGY_V_DRUG_INGREDIENTS, GMR_ALLERGY_V_VA_DRUG_CLASS from PATIENT_ALLERGIES where GMR_ALLERGY_V_GMR_ALLERGIES IS NULL LIMIT 10;

-- Query that shows a JOIN based on the pointer fields
select o.OBJECT_OF_ORDER_V_PATIENT, p.NAME from ORDER1 o, PATIENT p WHERE o.OBJECT_OF_ORDER_V_PATIENT = p.PATIENT_ID LIMIT 10;
