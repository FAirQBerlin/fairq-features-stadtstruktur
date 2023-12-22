select
  standalter,
  baumhoehe,
  geometry
from 
  stadtstruktur_trees_street
union distinct
select
  standalter,
  baumhoehe,
  geometry
from
  stadtstruktur_trees_park;
