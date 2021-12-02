-- ######################################################################
-- #									#
-- # Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

CREATE TABLE stock_availability (product_id INT PRIMARY KEY, available BOOLEAN);

INSERT INTO stock_availability (product_id, available)
VALUES
   (100, TRUE),
   (200, FALSE),
   (300, TRUE),
   (400, TRUE),
   (500, TRUE),
   (600, TRUE),
   (700, FALSE),
   (800, FALSE);

