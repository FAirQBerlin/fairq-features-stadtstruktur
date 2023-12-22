-- fairq_features.streets definition
CREATE TABLE fairq_features.streets
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `strassenklasse_0` Float64,
    `strassenklasse_I` Float64,
    `strassenklasse_II` Float64,
    `strassenklasse_III` Float64,
    `strassenklasse_IV` Float64,
    `strassenklasse_V` Float64
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.traffic_intensity
CREATE TABLE fairq_features.traffic_intensity
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `traffic_intensity_kfz` Float64,
    `traffic_intensity_lkw` Float64
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.traffic_volume
CREATE TABLE fairq_features.traffic_volume
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `kfz_per_24h` Int32
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.land_use definition
CREATE TABLE fairq_features.land_use
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `gewaesser` Float64,
    `grauflaeche` Float64,
    `gruenflaeche` Float64,
    `infrastruktur` Float64,
    `mischnutzung` Float64,
    `wald` Float64,
    `wohnnutzung` Float64
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

create table fairq_features.coord_mapping_emissions_stadt (
 stadt_x Int32 COMMENT 'mapping stadt grid x coordinate',
 stadt_y Int32 COMMENT 'mapping stadt grid y coordinate',
 emissions_x Int32 COMMENT 'mapping emissions x coordinate',
 emissions_y Int32 COMMENT 'mapping emissions y coordinate',
 idnr_1km Int64 COMMENT 'id emissions'
) ENGINE = ReplacingMergeTree
ORDER BY (stadt_x, stadt_y)
COMMENT 'Code to fill this table can be found at https://github.com/INWT/fairq-features-stadtstruktur/blob/main/inst/db/schema.sql';


insert into fairq_features.coord_mapping_emissions_stadt
with coords_emissions as (
 select distinct
  idnr_1km,
  (x_max + x_min) / 2 as x,  
  (y_max + y_min) / 2 as y 
 from 
  fairq_raw.stadtstruktur_emissions
),
 dists as (
 select
   idnr_1km,
   coords_emissions.x,
   coords_emissions.y,
   coord_stadt_all.x as stadt_x,
   coord_stadt_all.y as stadt_y,
   L2Distance((coords_emissions.x, coords_emissions.y), (coord_stadt_all.x, coord_stadt_all.y)) AS dist
from
 coords_emissions
CROSS JOIN
 fairq_features.coord_stadt_all
 )
select
 stadt_x,
 stadt_y,
 argMin((x, y), dist).1 as emissions_x,
 argMin((x, y), dist).2 as emissions_y,
 argMin(idnr_1km, dist) as idnr_1km
from
 dists
group by stadt_x, stadt_y;

create view fairq_features.emissions as 
select 
  stadt_x as x,
  stadt_y as y,
  nox_h_15, 
  nox_i_15, 
  nox_v_gn15, 
  nox_v_hn15, 
  nox_v_nn15, 
  nox_ge_15, 
  pm10_h_15, 
  pm10_i_15, 
  pm10_vgn15, 
  pm10_vhn15, 
  pm10_vnn15, 
  pm10_ge_15, 
  pm2_5_h_15, 
  pm2_5_i_15, 
  pm25_vgn15, 
  pm25_vhn15, 
  pm25_vnn15, 
  pm25_ge15
from 
  fairq_features.coord_mapping_emissions_stadt
left join
  fairq_raw.stadtstruktur_emissions using(idnr_1km);

-- fairq_features.buildings
CREATE TABLE fairq_features.buildings
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `density` Float64,
    `height` Float64
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.mapping_reprojection definition
CREATE TABLE fairq_features.mapping_reprojection
(
    `lat_int` Int32,
    `lon_int` Int32,
    `x` Int32,
    `y` Int32
)
ENGINE = ReplacingMergeTree()
ORDER BY (lat_int, lon_int);

-- fairq_features.trees
CREATE TABLE fairq_features.trees
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `count` Float64,
    `age` Nullable(Float64),
    `height` Nullable(Float64)
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.coord_stadt_berlin
CREATE TABLE fairq_features.coord_stadt_berlin
(
    `id` Int32,
    `x` Int32,
    `y` Int32
)
ENGINE = ReplacingMergeTree()
ORDER BY id;

-- fairq_features.coord_stadt_streets
CREATE TABLE fairq_features.coord_mapping_stadt_streets
(
    `element_nr` String COMMENT 'id of street section of Detailnetz Gesamt',
    `geometry` String COMMENT 'coordinates of street section as LINESTRING geojson format in EPSG:25833',
    `stadt_x` Int32 COMMENT 'mapping stadt grid x coordinate',
    `stadt_y` Int32 COMMENT 'mapping stadt grid y coordinate'
)
ENGINE = ReplacingMergeTree()
ORDER BY (element_nr, stadt_x, stadt_y);

-- fairq_features.coord_stadt_lor
CREATE TABLE fairq_features.coord_mapping_stadt_lor
(
    `PLR_ID` String COMMENT 'id of LOR Planungsraum - 8 digits',
    `stadt_x` Int32 COMMENT 'mapping stadt grid x coordinate',
    `stadt_y` Int32 COMMENT 'mapping stadt grid y coordinate'
)
ENGINE = ReplacingMergeTree()
ORDER BY (PLR_ID, stadt_x, stadt_y);

-- fairq_features.mapping_reprojection definition
CREATE TABLE fairq_features.coord_mapping_stadt_reprojection
(
    `id` Int32,
    `x` Int32,
    `y` Int32,
    `lon_int` Int32,
    `lat_int` Int32
)
ENGINE = ReplacingMergeTree()
ORDER BY (id);
