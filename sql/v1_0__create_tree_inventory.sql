CREATE TABLE tree_inventory (
    treeid INTEGER PRIMARY KEY
  , common_name VARCHAR(50)
  , age_class VARCHAR(50)
  , canopy_radius NUMERIC(3,1)
  , condition_class VARCHAR(50)
  , desirability NUMERIC(3,2)
  , height_class VARCHAR(50)
  , overhead_lines VARCHAR(50)
  , planting_type INTEGER
  , root_infringement VARCHAR(50)
  , stems INTEGER
  , gtw_priority INTEGER
  , estimated_value NUMERIC(8,2)
  , tree_notes VARCHAR(100)
  , est_pruning_hours INTEGER
  , crew VARCHAR(50)
  , date_inventoried DATE
  , last_updated DATE
  , other_species VARCHAR(50)
  , i_tree_code VARCHAR(50)
  , genus_species VARCHAR(50)
  , genus VARCHAR(50)
  , species VARCHAR(50)
  , latitude NUMERIC(10,8)
  , location_type VARCHAR(50)
  , location_value VARCHAR(50)
  , longitude NUMERIC(11,8)
  , dedicated VARCHAR(50)
  , dedication_year INTEGER
  , dbh_1 INTEGER
  , dbh_2 INTEGER
  , dbh_3 INTEGER
  , dbh_4 INTEGER
  , dbh_5 INTEGER
  , dbh_height NUMERIC(3,2)
  , adv_ass_root BOOLEAN
  , adv_ass_stem BOOLEAN
  , vtsa BOOLEAN
  , isa_consequences VARCHAR(50)
  , isa_failure VARCHAR(50)
  , isa_fail_impact VARCHAR(50)
  , isa_impact VARCHAR(50)
  , isa_risk_rating VARCHAR(50)
  , plant_part_concern VARCHAR(50)
  , primary_target VARCHAR(50)
  , secondary_target VARCHAR(50)
  , date_rec TIMESTAMP
  , rcx BOOLEAN
  , soils_1 VARCHAR(50)
  , soils_2 VARCHAR(50)
  , date_rec_1 TIMESTAMP
  , prune BOOLEAN
  , pruning_category VARCHAR(50)
  , pruning_goal_1 VARCHAR(50)
  , pruning_goal_2 VARCHAR(50)
  , pruning_goal_other VARCHAR(50)
  , pruning_system VARCHAR(50)
  , date_rec_2 TIMESTAMP
  , removal VARCHAR(50)
  , date_rec_3 TIMESTAMP
  , cable BOOLEAN
  , cable_info VARCHAR(50)
  , cable_size VARCHAR(50)
  , date_rec_4 TIMESTAMP
  , brace_rod BOOLEAN
  , brace_rod_info VARCHAR(50)
  , date_rec_5 TIMESTAMP
  , date_rec_6 TIMESTAMP
  , guy BOOLEAN
  , guy_info VARCHAR(50)
  , date_rec_7 TIMESTAMP
  , date_rec_8 TIMESTAMP
  , pest_disease_type_1 VARCHAR(50)
  , pest_disease_type_2 VARCHAR(50)
  , pest_disease_type_3 VARCHAR(50)
  , pest_disease_type_4 VARCHAR(50)
  , pest_disease_type_5 VARCHAR(50)
  , pest_disease_type_6 VARCHAR(50)
  , defect_other VARCHAR(50)
  , defect_type_1 VARCHAR(50)
  , defect_type_2 VARCHAR(50)
  , defect_type_3 VARCHAR(50)
  , defect_type_4 VARCHAR(50)
  , defect_type_5 VARCHAR(50)
  , defect_type_6 VARCHAR(50)
  , addr_1 VARCHAR(50)
  , street_num VARCHAR(50)
  , street_name VARCHAR(50)
  , city VARCHAR(50)
  , post_code INTEGER
  , state VARCHAR(50)
  , crn_light_exposure INTEGER
  , total_height INTEGER
  , tree_site BOOLEAN
  , file_extension VARCHAR(50)
  , filename VARCHAR(100)
  , image_type VARCHAR(50)
)
;

COPY tree_inventory
FROM '/Users/kkirsch/prj/github/shade-tree-commission/data/sewickley_trees.csv'
DELIMITER ','
CSV HEADER;
