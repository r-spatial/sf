library(RPostgreSQL)
library(sf)
if (Sys.getenv("USER") %in% c("travis", "edzer")) {
  library(sp)
  data(meuse)
  sf = st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
  library(RPostgreSQL)
  conn = dbConnect(PostgreSQL(), dbname = "postgis")
  st_read_db(conn, query = "select * from meuse limit 3;")
  st_write_db(conn, sf, "meuse_tbl", dropTable = FALSE)
  dbDisconnect(conn)
}

options(warn = 2) # turn into error
if (Sys.getenv("USER") %in% c("edzer", "travis")) {
# if (Sys.getenv("USER") %in% c("edzer")) {
  cn = dbConnect(PostgreSQL(), dbname = "postgis")
  round_trip = function(cn, wkt) {
  	query = paste0("SELECT '", wkt, "'::geometry;")
	returnstr = suppressWarnings(dbGetQuery(cn, query)$geometry)
	# print(returnstr)
  	n = nchar(returnstr)/2
    wkb = lapply(returnstr, function(y) as.raw(as.numeric(paste0("0x", 
  	  sapply(1:n, function(x) substr(y, (x-1)*2+1, x*2))))))
  	class(wkb) = "WKB"
    ret = st_as_sfc(wkb, EWKB = TRUE)
	cat(returnstr, "\n")
    cat(paste(wkt, "<-->", st_as_text(ret, EWKT=TRUE)[[1]], "\n"))
	invisible(ret)
  }
  round_trip(cn, "SRID=4326;POINTM(0 0 0)")
  round_trip(cn, "POINTZ(0 0 0)")
  round_trip(cn, "POINTZM(0 0 0 0)")
  round_trip(cn, "POINT(0 0)")
  round_trip(cn, "LINESTRING(0 0,1 1,2 2)")
  round_trip(cn, "MULTIPOINT(0 0,1 1,2 2)")
  round_trip(cn, "POLYGON((0 0,1 0,1 1,0 0))")
  round_trip(cn, "MULTIPOLYGON(((0 0,1 0,1 1,0 0)),((2 2,3 2,3 3,2 2)))")
  round_trip(cn, "MULTIPOLYGON(((0 0,1 0,1 1,0 0),(.2 .2,.8 .2, .8 .8, .2 .2)),((2 2,3 2,3 3,2 2)))")
  round_trip(cn, "MULTILINESTRING((0 0,1 0,1 1,0 0),(.2 .2,.8 .2, .8 .8, .2 .2),(2 2,3 2,3 3,2 2))")


  #options(warn = -1)
  #query = paste0("SELECT wkb_geometry from meuse limit 2;")
  #wkb = as.list(dbGetQuery(cn, query)$wkb_geometry)
  #class(wkb) = "WKB"
  #ret = st_as_sfc(wkb, EWKB = TRUE)
  #ret 

  #m = st_read_db(cn, query = "select * from meuse;")
  dbDisconnect(cn)
}
