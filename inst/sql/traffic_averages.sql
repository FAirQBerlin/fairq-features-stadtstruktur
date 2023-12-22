select
 strklasse1 as strassenklasse,
 avg(dtvw_kfz) as kfz,
 avg(dtvw_lkw) as lkw
from 
  stadtstruktur_traffic
group by
 strklasse1;
