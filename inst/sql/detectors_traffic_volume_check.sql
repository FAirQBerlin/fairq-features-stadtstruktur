select
distinct
	tdcsp.mq_short_name mq_name,
	tdcsp.position,
	tdcsp.pos_detail,
	tdcsp.lat,
	tdcsp.lon,
	mr.lat_int,
	mr.lon_int,
	mr.x det_x,
	mr.y det_y,
	traffic_vol.x,
	traffic_vol.y
from
	fairq_raw.traffic_det_cross_sections_processed tdcsp
	-- Add mapping from lat/lon to x/y
inner join
 fairq_features.mapping_reprojection mr on
	(tdcsp.lat_int = mr.lat_int
		and tdcsp.lon_int = mr.lon_int)
	-- Add mapping to closest stadstruktur coordinate
left join
  fairq_features.coord_mapping_stadt_det cmsd on
	(mr.x = cmsd.det_x
		and mr.y = cmsd.det_y)
left join
  fairq_features.traffic_volume traffic_vol on
	(cmsd.stadt_x = traffic_vol.x
		and cmsd.stadt_y = traffic_vol.y)
where
	kfz_per_24h = 0;
