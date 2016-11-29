library(sf)
library(dplyr)
nc = st_read(system.file("shape/nc.shp", package="sf"))
nc %>% filter(AREA > .1) %>% plot()

# plot 10 smallest counties in grey:
nc %>% plot()
nc %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')

# select: check both when geometry is part of the selection, and when not:
nc %>% select(SID74, SID79) %>% names()
nc %>% select(SID74, SID79, geometry) %>% names()
nc %>% select(SID74, SID79) %>% class()
nc %>% select(SID74, SID79, geometry) %>% class()

# arrange: ten smallest counties
nc %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')

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
nc %>% select(SID74, SID79, geometry) %>% gather(VAR, SID, -geometry) %>% summary()

# spread:
nc$row = 1:100
nc.g <- nc %>% select(SID74, SID79, row) %>% gather(VAR, SID, -row)
nc.g %>% tail()
nc.g %>% spread(VAR, SID) %>% head()
nc %>% select(SID74, SID79, geometry, row) %>% gather(VAR, SID, -geometry, -row) %>% spread(VAR, SID) %>% head()


library(dplyr)
library(sf)
demo(nc, ask = FALSE, echo = FALSE)
nc.merc <- st_transform(nc, 3857) # web mercator - not so good!
nc.merc <- nc.merc %>% mutate(area = st_area(nc.merc), dens = BIR74/area)
summary(nc.merc$dens)
nc.merc$area_cl <- cut(nc$AREA, c(0, .1, .12, .15, .25))
nc.grp <- nc.merc %>% group_by(area_cl)
out <- nc.grp %>% summarise(A = sum(area), pop = sum(dens * area), new_dens = pop/A) 
out %>% summarise(sum(A * new_dens))
nc.merc %>% summarise(sum(area * dens))

sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
x <- sfc %>% st_set_crs(4326) %>% st_transform(3857)
x

