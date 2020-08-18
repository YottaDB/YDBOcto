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

-- TMF01 : OCTO574 : names schema : Using ROUND() on AVG() aggregate function in HAVING clause returns incorrect results

SELECT ROUND(AVG(id),11) FROM names GROUP BY firstname HAVING ROUND(AVG(id),11) IN (3);
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING AVG(id) IN (3);
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING ROUND(AVG(id),1) IN (3);

SELECT ROUND(AVG(id),11) FROM names GROUP BY firstname HAVING ROUND(AVG(id),11) NOT IN (3);
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING AVG(id) NOT IN (3);
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING ROUND(AVG(id),1) NOT IN (3);

SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING AVG(id) = 3;
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING ROUND(AVG(id),1) = 3;

SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING AVG(id) != 3;
SELECT AVG(id),round(avg(id),1) FROM names GROUP BY firstname HAVING ROUND(AVG(id),1) != 3;

