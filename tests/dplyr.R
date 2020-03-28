suppressPackageStartupMessages(library(sf))

library(dplyr)
read_sf(system.file("shape/nc.shp", package="sf"), quiet = TRUE) %>%
	st_transform(3857) -> nc
nc %>% filter(AREA > .1) %>% plot()

# plot 10 smallest counties in grey:
nc %>% 
  select(BIR74, geometry) %>% 
  plot()

nc %>% 
  select(AREA, geometry) %>% 
  arrange(AREA) %>% 
  slice(1:10) %>% 
  plot(add = TRUE, col = 'grey', main ="")

# select: check both when geometry is part of the selection, and when not:
nc %>% select(SID74, SID79) %>% names()
nc %>% select(SID74, SID79, geometry) %>% names()
nc %>% select(SID74, SID79) %>% class()
nc %>% select(SID74, SID79, geometry) %>% class()

# group_by:
nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
nc %>% group_by(area_cl) %>% class()

# mutate:
nc2 <- nc %>% mutate(area10 = AREA/10)

# transmute:
nc %>% transmute(AREA = AREA/10, geometry = geometry) %>% class()
nc %>% transmute(AREA = AREA/10) %>% class()

# rename:
nc2 <- nc %>% rename(area = AREA)

# distinct:
nc[c(1:100,1:10),] %>% distinct() %>% nrow()

# summarize:
nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
nc.g <- nc %>% group_by(area_cl)
nc.g %>% summarise(mean(AREA))
nc.g %>% summarize(mean(AREA)) %>% plot(col = 3:6/7)

library(tidyr)

# time-wide to long table, using tidyr::gather
# stack the two SID columns for the July 1, 1974 - June 30, 1978 and July 1, 1979 - June 30, 1984 periods
# (see https://cran.r-project.org/web/packages/spdep/vignettes/sids.pdf)
nc %>% select(SID74, SID79, geometry) %>% gather("VAR", "SID", -geometry) %>% summary()

# spread:
nc$row = 1:100
nc.g <- nc %>% select(SID74, SID79, row) %>% gather("VAR", "SID", -row, -geometry)
nc.g %>% tail()
nc.g %>% spread(VAR, SID) %>% head()
nc %>% select(SID74, SID79, geometry, row) %>% gather("VAR", "SID", -geometry, -row) %>% spread(VAR, SID) %>% head()

# test st_set_crs in pipe:
sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
x <- sfc %>% st_set_crs(4326) %>% st_transform(3857)
x

read_sf(system.file("shape/nc.shp", package="sf"), quiet = TRUE) %>%
	st_transform(3857) -> nc
nc.merc <- st_transform(nc, 32119) # NC State Plane
suppressPackageStartupMessages(library(units))
install_symbolic_unit("person")
person = as_units("person")
nc.merc <- nc.merc %>% mutate(area = st_area(nc.merc), dens = BIR74 * person / area)

# summary(nc.merc$dens) # requires units 0.4-2
nc.merc$area_cl <- cut(nc$AREA, c(0, .1, .12, .15, .25))
nc.grp <- nc.merc %>% group_by(area_cl)

out <- nc.grp %>% summarise(A = sum(area), pop = sum(dens * area), 
	new_dens = sum(dens * area)/sum(area)) 

# mean densities depend on grouping:
nc.merc %>% summarize(mean(dens))
out %>% summarise(mean(new_dens))

# total densities don't:
nc.merc %>% summarise(sum(area * dens))
out %>% summarise(sum(A * new_dens))

conn = system.file("gpkg/nc.gpkg", package = "sf")

library(DBI)
library(RSQLite)
con = dbConnect(SQLite(), dbname = system.file("gpkg/nc.gpkg", package = "sf"))
dbReadTable(con, "nc.gpkg") %>% filter(AREA > 0.2) %>% collect %>% st_sf

# nest:
storms.sf = st_as_sf(storms, coords = c("long", "lat"), crs = 4326)
x <- storms.sf %>% group_by(name, year) %>% nest

nrow(distinct(nc[c(1,1,1,2,2,3:100),]))

# set.seed(1331)
nc$gp <- sample(10, 100, replace=TRUE)
# Get centroid of each group of polygons; https://github.com/r-spatial/sf/issues/969
nc_gp_cent <- nc %>%
                group_by(gp) %>%
                group_map(st_area)

nc %>% st_filter(nc[1,]) %>% nrow
