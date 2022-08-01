#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# YDBOctoVistA#7 Refactoring Word Processing Extract Fields to use DELIM ''
# The field whose definition got changed from EXTRACT to DELIM '' is TIU_DOCUMENT_REPORT_TEXT.REPORT_TEXT
SELECT P.NAME, T.PATIENT, X.REPORT_TEXT from PATIENT P, TIU_DOCUMENT T, TIU_DOCUMENT_REPORT_TEXT X WHERE T.PATIENT = P.PATIENT_ID and T.TIU_DOCUMENT_ID = X.TIU_DOCUMENT_ID LIMIT 100;
