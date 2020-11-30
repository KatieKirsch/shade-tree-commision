CREATE TABLE planting_site (
    siteid SERIAL PRIMARY KEY
  , latitude NUMERIC(10,8)
  , longitude NUMERIC(11,8)
  , addr_1 VARCHAR(50)
  , street_num VARCHAR(50)
  , street_name VARCHAR(50)
  , city VARCHAR(50)
  , post_code INTEGER
  , state VARCHAR(50)
)
;
