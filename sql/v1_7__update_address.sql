UPDATE tree_inventory AS t
SET
    street_num = t2.street_num
  , addr_1 = t2.addr_1
FROM (VALUES
  -- afrishkorn
   (281,14,'14 Beaver St')
  ,(283,2,'2 Beaver St')
  ,(284,3,'3 Beaver St')
  ,(286,17,'17 Beaver St')
  ,(1544,NULL,'Beaver St')
  ,(1546,NULL,'Beaver St')
  ,(1549,NULL,'Beaver St')
) AS t2(treeid, street_num, addr_1)
WHERE t2.treeid = t.treeid;
