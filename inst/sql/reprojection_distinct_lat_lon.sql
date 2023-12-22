
select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.cams_processed

UNION DISTINCT

select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.cams_old_processed

UNION DISTINCT

select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.dwd_observations_processed

UNION DISTINCT

select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.dwd_forecasts_processed

UNION DISTINCT

select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.stadtstruktur_measuring_stations_processed

UNION DISTINCT

select lat_int / 100000 as lat, lon_int / 100000 as lon from fairq_raw.traffic_det_cross_sections_processed;
