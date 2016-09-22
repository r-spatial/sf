library(sp)
demo(meuse, ask = FALSE, echo = FALSE)
library(sf)
meuse = st_as_sf(meuse)
# write shapefile:
st_write(meuse, ".", "meuse")
library(RPostgreSQL)
# -- doesn't work on appveyor:
#cn = dbConnect(PostgreSQL(), dbname = "postgis")
#st_write_db(cn, meuse, "meusedb", dropTable = FALSE)
#st_write_db(cn, meuse, "meusedb", dropTable = TRUE, wkb = FALSE)
#dbDisconnect(cn)
