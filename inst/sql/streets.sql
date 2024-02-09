select
  strassenklasse,
  strassenklasse1,
  element_nr,
  geometry,
  dtvw_kfz
from stadtstruktur_network_streets
left join fairq_raw.stadtstruktur_traffic on(element_nr = elem_nr);

