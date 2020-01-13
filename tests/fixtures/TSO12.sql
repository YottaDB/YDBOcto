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

-- TSO12 : OCTO268 : Check if each query in the UNION operator has the same number and/or type of columns

(select id        from names) union (select 1+id      from names);
(select id        from names) union (select firstname from names);
(select 1+id      from names) union (select id        from names);
(select 1+id      from names) union (select firstname from names);
(select firstname from names) union (select id        from names);
(select firstname from names) union (select 1+id      from names);
(select id        from names) union (select id,1+id        from names);
(select 1+id      from names) union (select id,firstname   from names);
(select firstname from names) union (select 1+id,id        from names);
(select id        from names) union (select 1+id,firstname from names);
(select 1+id      from names) union (select firstname,id   from names);
(select firstname from names) union (select firstname,1+id from names);
(select id        from names) union (select firstname,lastname,id from names);
(select 1+id      from names) union (select lastname,id,firstname from names);
(select firstname from names) union (select *                     from names);
(select id,1+id        from names) union (select id        from names);
(select id,firstname   from names) union (select 1+id      from names);
(select 1+id,id        from names) union (select firstname from names);
(select 1+id,firstname from names) union (select id        from names);
(select firstname,id   from names) union (select 1+id      from names);
(select firstname,1+id from names) union (select firstname from names);
(select firstname,lastname from names) union (select firstname,1+id from names);
(select * from (select 1+id from names))                                               union (select firstname from names);
(select newid from (select 1+id as newid from names))                                  union (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) union (select firstname from names);
(select firstname from names) union (select * from (select 1+id from names))                                              ;
(select firstname from names) union (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) union (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));
(select id        from names) union all (select 1+id      from names);
(select id        from names) union all (select firstname from names);
(select 1+id      from names) union all (select id        from names);
(select 1+id      from names) union all (select firstname from names);
(select firstname from names) union all (select id        from names);
(select firstname from names) union all (select 1+id      from names);
(select id        from names) union all (select id,1+id        from names);
(select 1+id      from names) union all (select id,firstname   from names);
(select firstname from names) union all (select 1+id,id        from names);
(select id        from names) union all (select 1+id,firstname from names);
(select 1+id      from names) union all (select firstname,id   from names);
(select firstname from names) union all (select firstname,1+id from names);
(select id        from names) union all (select firstname,lastname,id from names);
(select 1+id      from names) union all (select lastname,id,firstname from names);
(select firstname from names) union all (select *                     from names);
(select id,1+id        from names) union all (select id        from names);
(select id,firstname   from names) union all (select 1+id      from names);
(select 1+id,id        from names) union all (select firstname from names);
(select 1+id,firstname from names) union all (select id        from names);
(select firstname,id   from names) union all (select 1+id      from names);
(select firstname,1+id from names) union all (select firstname from names);
(select firstname,lastname from names) union all (select firstname,1+id from names);
(select * from (select 1+id from names))                                               union all (select firstname from names);
(select newid from (select 1+id as newid from names))                                  union all (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) union all (select firstname from names);
(select firstname from names) union all (select * from (select 1+id from names))                                              ;
(select firstname from names) union all (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) union all (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));
(select id        from names) intersect (select 1+id      from names);
(select id        from names) intersect (select firstname from names);
(select 1+id      from names) intersect (select id        from names);
(select 1+id      from names) intersect (select firstname from names);
(select firstname from names) intersect (select id        from names);
(select firstname from names) intersect (select 1+id      from names);
(select id        from names) intersect (select id,1+id        from names);
(select 1+id      from names) intersect (select id,firstname   from names);
(select firstname from names) intersect (select 1+id,id        from names);
(select id        from names) intersect (select 1+id,firstname from names);
(select 1+id      from names) intersect (select firstname,id   from names);
(select firstname from names) intersect (select firstname,1+id from names);
(select id        from names) intersect (select firstname,lastname,id from names);
(select 1+id      from names) intersect (select lastname,id,firstname from names);
(select firstname from names) intersect (select *                     from names);
(select id,1+id        from names) intersect (select id        from names);
(select id,firstname   from names) intersect (select 1+id      from names);
(select 1+id,id        from names) intersect (select firstname from names);
(select 1+id,firstname from names) intersect (select id        from names);
(select firstname,id   from names) intersect (select 1+id      from names);
(select firstname,1+id from names) intersect (select firstname from names);
(select firstname,lastname from names) intersect (select firstname,1+id from names);
(select * from (select 1+id from names))                                               intersect (select firstname from names);
(select newid from (select 1+id as newid from names))                                  intersect (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) intersect (select firstname from names);
(select firstname from names) intersect (select * from (select 1+id from names))                                              ;
(select firstname from names) intersect (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) intersect (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));
(select id        from names) intersect all (select 1+id      from names);
(select id        from names) intersect all (select firstname from names);
(select 1+id      from names) intersect all (select id        from names);
(select 1+id      from names) intersect all (select firstname from names);
(select firstname from names) intersect all (select id        from names);
(select firstname from names) intersect all (select 1+id      from names);
(select id        from names) intersect all (select id,1+id        from names);
(select 1+id      from names) intersect all (select id,firstname   from names);
(select firstname from names) intersect all (select 1+id,id        from names);
(select id        from names) intersect all (select 1+id,firstname from names);
(select 1+id      from names) intersect all (select firstname,id   from names);
(select firstname from names) intersect all (select firstname,1+id from names);
(select id        from names) intersect all (select firstname,lastname,id from names);
(select 1+id      from names) intersect all (select lastname,id,firstname from names);
(select firstname from names) intersect all (select *                     from names);
(select id,1+id        from names) intersect all (select id        from names);
(select id,firstname   from names) intersect all (select 1+id      from names);
(select 1+id,id        from names) intersect all (select firstname from names);
(select 1+id,firstname from names) intersect all (select id        from names);
(select firstname,id   from names) intersect all (select 1+id      from names);
(select firstname,1+id from names) intersect all (select firstname from names);
(select firstname,lastname from names) intersect all (select firstname,1+id from names);
(select * from (select 1+id from names))                                               intersect all (select firstname from names);
(select newid from (select 1+id as newid from names))                                  intersect all (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) intersect all (select firstname from names);
(select firstname from names) intersect all (select * from (select 1+id from names))                                              ;
(select firstname from names) intersect all (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) intersect all (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));
(select id        from names) except (select 1+id      from names);
(select id        from names) except (select firstname from names);
(select 1+id      from names) except (select id        from names);
(select 1+id      from names) except (select firstname from names);
(select firstname from names) except (select id        from names);
(select firstname from names) except (select 1+id      from names);
(select id        from names) except (select id,1+id        from names);
(select 1+id      from names) except (select id,firstname   from names);
(select firstname from names) except (select 1+id,id        from names);
(select id        from names) except (select 1+id,firstname from names);
(select 1+id      from names) except (select firstname,id   from names);
(select firstname from names) except (select firstname,1+id from names);
(select id        from names) except (select firstname,lastname,id from names);
(select 1+id      from names) except (select lastname,id,firstname from names);
(select firstname from names) except (select *                     from names);
(select id,1+id        from names) except (select id        from names);
(select id,firstname   from names) except (select 1+id      from names);
(select 1+id,id        from names) except (select firstname from names);
(select 1+id,firstname from names) except (select id        from names);
(select firstname,id   from names) except (select 1+id      from names);
(select firstname,1+id from names) except (select firstname from names);
(select firstname,lastname from names) except (select firstname,1+id from names);
(select * from (select 1+id from names))                                               except (select firstname from names);
(select newid from (select 1+id as newid from names))                                  except (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) except (select firstname from names);
(select firstname from names) except (select * from (select 1+id from names))                                              ;
(select firstname from names) except (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) except (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));
(select id        from names) except all (select 1+id      from names);
(select id        from names) except all (select firstname from names);
(select 1+id      from names) except all (select id        from names);
(select 1+id      from names) except all (select firstname from names);
(select firstname from names) except all (select id        from names);
(select firstname from names) except all (select 1+id      from names);
(select id        from names) except all (select id,1+id        from names);
(select 1+id      from names) except all (select id,firstname   from names);
(select firstname from names) except all (select 1+id,id        from names);
(select id        from names) except all (select 1+id,firstname from names);
(select 1+id      from names) except all (select firstname,id   from names);
(select firstname from names) except all (select firstname,1+id from names);
(select id        from names) except all (select firstname,lastname,id from names);
(select 1+id      from names) except all (select lastname,id,firstname from names);
(select firstname from names) except all (select *                     from names);
(select id,1+id        from names) except all (select id        from names);
(select id,firstname   from names) except all (select 1+id      from names);
(select 1+id,id        from names) except all (select firstname from names);
(select 1+id,firstname from names) except all (select id        from names);
(select firstname,id   from names) except all (select 1+id      from names);
(select firstname,1+id from names) except all (select firstname from names);
(select firstname,lastname from names) except all (select firstname,1+id from names);
(select * from (select 1+id from names))                                               except all (select firstname from names);
(select newid from (select 1+id as newid from names))                                  except all (select firstname from names);
(select new2id from (select newid+2 as new2id from (select 1+id as newid from names))) except all (select firstname from names);
(select firstname from names) except all (select * from (select 1+id from names))                                              ;
(select firstname from names) except all (select newid from (select 1+id as newid from names))                                 ;
(select firstname from names) except all (select new2id from (select newid+2 as new2id from (select 1+id as newid from names)));

SELECT a.id as A_id, b.id as B_id
FROM names a INNER JOIN names b ON a.firstName = b.firstName
UNION
SELECT id as A_id, "" AS B_id
FROM names a
WHERE a.firstName NOT IN (SELECT b.firstName FROM names b);

SELECT *
FROM names n1
INNER JOIN names n2
ON n1.id = n2.id
UNION ALL
(
  SELECT id, firstName, lastName, '', '', ''
  FROM names
  EXCEPT ALL
  SELECT n1.id, n1.firstName, n1.lastName, '', '', ''
  FROM names n1
  INNER JOIN names n2
  ON n1.id = n2.id
);

