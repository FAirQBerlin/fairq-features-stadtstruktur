WITH all_coords AS
  (SELECT x,
          y,
          L2Distance((stadt_x, stadt_y), (x, y)) dist
   FROM fairq_features.coord_mapping_stadt_passive
   CROSS JOIN fairq_features.coord_mapping_stadt_reprojection
   WHERE dist < 71 )
SELECT cmsr.id id_stadt,
       cmsr.lon_int / 100000 lon_stadt,
       cmsr.lat_int / 100000 lat_stadt,
       ms.id id_passive,
       ms.lat lat_passive,
       ms.lon lon_passive,
       ms.adresse address,
       fairq_features.traffic_model_scaling.kfz_per_24h kfz_per_24h
FROM all_coords ac
LEFT JOIN fairq_features.traffic_model_scaling USING(x, y)
LEFT JOIN fairq_features.coord_mapping_stadt_passive p ON (
      ac.x = stadt_x
      AND ac.y = stadt_y)
LEFT JOIN fairq_raw.stadtstruktur_measuring_stations ms ON p.id = ms.id
LEFT JOIN fairq_features.coord_mapping_stadt_reprojection cmsr ON (
      ac.x = cmsr.x
      AND ac.y = cmsr.y) 
      settings join_use_nulls=1;
