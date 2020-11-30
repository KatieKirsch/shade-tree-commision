ALTER TABLE tree_inventory
ADD COLUMN removed_flg INTEGER;

UPDATE tree_inventory
SET removed_flg = 0;
