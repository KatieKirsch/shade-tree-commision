ALTER TABLE tree_inventory
ADD CONSTRAINT check_types
CHECK (condition_class IN ('Good', 'Poor', 'Dead', 'Fair'));
