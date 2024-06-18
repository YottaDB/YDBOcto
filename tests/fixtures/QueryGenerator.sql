#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(NUMERIC) RETURNS NUMERIC AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(VARCHAR) RETURNS VARCHAR AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(BOOLEAN) RETURNS BOOLEAN AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(DATE) RETURNS DATE AS $$samevalue^functions;

