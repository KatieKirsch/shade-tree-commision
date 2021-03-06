UPDATE tree_inventory AS t
SET
    condition_class = t2.condition_class
FROM (VALUES
  -- bfinley
   (248,'Poor')
  ,(249,'Poor')
  ,(320,'Poor')
  ,(770,'Poor')
  ,(777,'Poor')
  -- cmullins
  ,(853,'Dead')
  ,(271,'Dead')
  ,(924,'Dead')
  ,(92,'Dead')
  ,(86,'Dead')
  ,(296,'Fair')
  ,(298,'Fair')
  ,(270,'Fair')
  ,(302,'Fair')
  ,(305,'Fair')
  ,(306,'Fair')
  ,(307,'Fair')
  ,(885,'Poor')
  ,(472,'Poor')
  ,(981,'Poor')
  ,(1033,'Poor')
  -- afrishkorn
  ,(859,'Dead')
  ,(1725,'Poor')
  ,(1737,'Poor')
  ,(883,'Fair')
  ,(884,'Fair')
  ,(820,'Dead')
  ,(816,'Poor')
  ,(815,'Good')
  ,(289,'Poor')
  ,(296,'Poor')
  ,(298,'Poor')
  ,(301,'Poor')
  ,(270,'Poor')
  ,(302,'Dead')
  ,(305,'Poor')
  ,(306,'Poor')
  ,(307,'Poor')
  ,(1217,'Dead')
  ,(308,'Poor')
  ,(1893,'Poor')
  ,(236,'Poor')
  ,(337,'Poor')
  ,(339,'Poor')
  ,(340,'Poor')
  ,(1441,'Poor')
  ,(698,'Good')
  ,(699,'Fair')
  ,(53,'Good')
  ,(446,'Poor')
  ,(1832,'Good')
  ,(1837,'Poor')
  ,(1543,'Good')
  ,(1551,'Good')
  ,(1554,'Good')
  ,(1544,'Good')
  ,(1573,'Good')
  ,(1574,'Good')
  ,(1575,'Good')
  ,(603,'Dead')
  ,(47,'Good')
  ,(1102,'Good')
  ,(1829,'Fair')
  ,(160,'Poor')
  ,(55,'Fair')
  ,(753,'Fair')
  ,(626,'Fair')
  ,(612,'Fair')
  ,(614,'Fair')
  ,(854,'Fair')
  ,(1379,'Fair')
  ,(1844,'Fair')
  ,(1845,'Fair')
  ,(1701,'Fair')
) AS t2(treeid, condition_class)
WHERE t2.treeid = t.treeid;
