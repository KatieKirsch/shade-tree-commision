UPDATE tree_inventory AS t
SET
    removed_flg = t2.removed_flg
FROM (VALUES
  -- cmullins
   (1725,1)
  ,(1175,1)
  ,(978,1)
  ,(1027,1)
  ,(859,1)
  ,(886,1)
  ,(1192,1)
  ,(1129,1)
  -- afrishkorn
  ,(885,1)
  ,(886,1)
  ,(823,1)
  ,(1367,1)
  ,(1129,1)
  ,(829,1)
) AS t2(treeid, removed_flg)
WHERE t2.treeid = t.treeid;
