# put as last, because of spatstat side effect on units:
require(spatstat)
require(sf)
data(chicago)
st_as_sf(chicago)
st_as_sf(chicago, both = FALSE)
