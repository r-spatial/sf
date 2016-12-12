# 
# Currently, only the first part is kept for conversion to the single-part types,
# but another option is to replicate the feature out for every part, though now I 
# think that's not a job for st_cast. 
# 
# Other related issues, 
# - should a linestring be automatically closed if not already, on cast to polygon
# - should a polygon closing coordinate be dropped on cast to point 
# - should multipoint be interpretable as linestring/polygon
# 
# I'll proceed with
# 
# - provide for all possible conversions in st_cast, with the next as caveat:
# - error on cases that result in loss (e.g. multi-ring multipolygon to polygon, 
#      but allow single-ring multipolygon to polygon)
# - auto-close linestrings turned into polygon - this is not possible currently with st_multipolygon
# - keep the duplicated polygon coordinate on conversion to multipoint (keep it for to-linestring obvs.)
# - allow multipoint native order conversion to linestring/polygon (but with a warning)
# 
# In the case of error on loss, suggest to find an alternative function that duplicates the features appropriately  (and that ... probably ... drops any holes when converting to polygon) 
# 
# 
# 1. keeping the polygon to point extra point means it's round-trip-able
# 


## generics needed
st_multpolygon <- function(x, ...) {
  UseMethod("st_multipolygon")
}
##' the empty or standard st_read nested list input
st_multipolygon.default <- function(x, ...){
  sf::st_multipolygon(x)
  #function(x = list(), dim = "XYZ") MtrxSetSet(x, dim, type = "MULTIPOLYGON", needClosed = TRUE)
}

st_multipolygon.MULTILINESTRING <- function(x, ...) {
  x <- relist_n(x, 1L)
  sf::st_multipolygon(x)
}





st_multipolygon(mls)
## note here that purrr wins
benchmark(purrr = {nc1 <- nc; nc1$geometry <- st_sfc(map(map(nc$geometry, 1:1), st_polygon))}, 
          mike = repl_g(mpl_to_pl, lapply(gmls, function(x) st_polygon(relist_0(x, 0L)[1L]))), replications = 100)
#test replications elapsed relative user.self sys.self user.child sys.child
#2  mike          100   2.777    1.081     2.776        0          0         0
#1 purrr          100   2.568    1.000     2.564        0          0         0

benchmark(purrr = {nc1 <- nc; nc1$geometry <- st_sfc(map(map(nc$geometry, 1), st_multilinestring))}, 
          mike = mpl_to_mls <- repl_g(nc, lapply(gmpl, function(x) st_multilinestring(relist_n(x, -1L)))), replications = 100)
#test replications elapsed relative user.self sys.self user.child sys.child
#2  mike          100   1.193    1.093     1.192        0          0         0
#1 purrr          100   1.091    1.000     1.092        0          0         0


#' op <- par(mfrow = c(2, 1))
#' plot(nc[w, "BIR79"]) 
#' plot(st_cast(nc[w, "BIR79"], "MULTIPOINT"))
#' par(op)
#'

list_up <- function(x) list(x)
list_down <- function(x) unlist(x, recursive = FALSE, use.names = FALSE)
relist_n <- function(x, n = 0) {
  if (n == 0) return(x)
  lister <- if(n > 0) list_up else list_down
  for (i in seq_len(abs(n))) x <- lister(x)
  x
}
## bind to one matrix (a multipoint)
relist_1 <- function(x, n = 0) {
  do.call(rbind, relist_n(x, n))
}
## keep only the first part
relist_0 <- function(x, n = 0) {
  y <- relist_n(x, n)
  if (length(y) > 1L) warning(sprintf("input %s has more than one part", rev(class(x)[2])))
  y##[[1L]]  ## drop a level of list (gets put back on for polygon, not for linestring)
}
library(sf)
example(st_read)
multis <- c(4L, 56L, 57L, 87L, 91L, 95L)
## just worker function temporarily for dev-testing
repl_g <- function(x, glist) {
  st_geometry(x) <- st_sfc(glist)
  x
}
gmpl <- st_geometry(nc)

#    MULTIPOLYGON MULTILINESTRING           relist_n  -1 
#    MULTIPOLYGON      MULTIPOINT           relist_1  -1 
#    MULTIPOLYGON         POLYGON           relist_0  -1  [1L] 
#    MULTIPOLYGON      LINESTRING           relist_0  -1  [[1L]]
#    MULTIPOLYGON           POINT           relist_0  -1  [[1L]] [1L, ]

mpl_to_mls <- repl_g(nc, lapply(gmpl, function(x) st_multilinestring(relist_n(x, -1L))))
mpl_to_mp <- repl_g(nc, lapply(gmpl, function(x) st_multipoint(relist_1(x, -1L))))
mpl_to_pl <- repl_g(nc, lapply(gmpl, function(x) st_polygon(relist_0(x, -1L)[1L])))
mpl_to_ls <- repl_g(nc, lapply(gmpl, function(x) st_linestring(relist_0(x, -1L)[[1L]])))
mpl_to_pt <- repl_g(nc, lapply(gmpl, function(x) st_point(relist_0(x, -1L)[[1L]][1L, ])))

plot(mpl_to_mls[multis, ])
plot(mpl_to_mp[multis, ])
plot(mpl_to_pl[multis, ])
plot(mpl_to_ls[multis, ])
plot(mpl_to_pt[multis, ])
plot(mpl_to_pt)

gmls <- st_geometry(mpl_to_mls)
# MULTILINESTRING    MULTIPOLYGON           relist_n 1
# MULTILINESTRING      MULTIPOINT           relist_1 0
# MULTILINESTRING         POLYGON           relist_0 0  [1]
# MULTILINESTRING      LINESTRING            relist_0 0 [[1]]
# MULTILINESTRING           POINT           relist_0  0 [[1L]] [1, ]


mls_to_mpl <- repl_g(mpl_to_mls, lapply(gmls, function(x) st_multipolygon(relist_n(x, 1L))))
mls_to_mp <- repl_g(mpl_to_mls, lapply(gmls, function(x) st_multipoint(relist_1(x, 0L))))
mls_to_pl <- repl_g(mpl_to_mls, lapply(gmls, function(x) st_polygon(relist_0(x, 0L)[1L])))
mls_to_ls <- repl_g(mpl_to_mls, lapply(gmls, function(x) st_linestring(relist_0(x, 0L)[[1L]])))
mls_to_pt <- repl_g(mpl_to_mls, lapply(gmls, function(x) st_point(relist_0(x, 0)[[1L]][1L, ])))

st_close <- function(x = list(), dim = "XYZ") sf:::MtrxSet(x, dim, type = "POLYGON", needClosed = F)
plot(mls_to_mpl)
plot(mls_to_mpl[multis, ])

plot(mls_to_mp)
plot(mls_to_mp[multis, ])

plot(mls_to_pl)
plot(mls_to_pl[multis, ])

plot(mls_to_pt)
plot(mls_to_pt[multis, ])



#       MULTIPOINT    MULTIPOLYGON
#       MULTIPOINT MULTILINESTRING
#       MULTIPOINT         POLYGON
#       MULTIPOINT      LINESTRING
#       MULTIPOINT           POINT

gmmp <- st_geometry(mpl_to_mp)
## multi-part to multipoint to multi-part won't well because the grouping is lost 
st_multipolygon_close <- function(x = list(), dim = "XYZ") sf:::MtrxSetSet(x, dim, type = "MULTIPOLYGON", needClosed = FALSE)
st_polygon_close <- function(x = list(), dim = "XYZ") sf:::MtrxSet(x, dim, type = "POLYGON", needClosed = FALSE)
mp_to_mpl <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_multipolygon_close(relist_n(x, 2L), dim = rev(class(x))[3])))
mp_to_mls <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_multilinestring(relist_n(x, 1L), dim = rev(class(x))[3])))
mp_to_pl <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_polygon_close(relist_n(unclass(x), 1), dim = rev(class(x))[3])))
mp_to_ls <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_linestring(relist_n(x, 0), dim = rev(class(x))[3])))
mp_to_pt <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_point(relist_0(x, 0)[1L, ], dim = rev(class(x))[3])))

## not failing fast: 
#mp_to_pl <- repl_g(mpl_to_mp, lapply(gmmp, function(x) st_polygon(relist_n(x, 1L), dim = rev(class(x))[3])))
##Error in Ops.sfg(head(y, 1), tail(y, 1)) : 
##  operation not supported for sfg objects 



  #          POLYGON    MULTIPOLYGON
  #          POLYGON MULTILINESTRING
  #          POLYGON      MULTIPOINT
  #          POLYGON      LINESTRING
  #          POLYGON           POINT

gmpl <- st_geometry(mpl_to_pl)
pl_to_mpl <- repl_g(mpl_to_pl, lapply(gmpl, function(x) st_multipolygon(relist_n(x, 1L))))
pl_to_mls <- repl_g(mpl_to_pl, lapply(gmpl, function(x) st_multilinestring(relist_n(x, 0L))))
pl_to_mp <- repl_g(mpl_to_pl, lapply(gmpl, function(x) st_multipoint(relist_n(x, 0)[[1]])))
pl_to_ls <- repl_g(mpl_to_pl, lapply(gmpl, function(x) st_linestring(relist_n(x, 0)[[1]])))
pl_to_pt <- repl_g(mpl_to_pl, lapply(gmpl, function(x) st_point(relist_0(x, 0)[[1L]][1L, ])))
  #       LINESTRING    MULTIPOLYGON
  #       LINESTRING MULTILINESTRING
  #       LINESTRING      MULTIPOINT
  #       LINESTRING         POLYGON
  #       LINESTRING           POINT
  #            POINT    MULTIPOLYGON
  #            POINT MULTILINESTRING
  #            POINT      MULTIPOINT
  #            POINT         POLYGON
  #            POINT      LINESTRING

             
# mk_level <- function(maxdepth = 2) {
#   force(maxdepth)
#   depth <- 0L
#   thefun <- function(x) {
#     depth <<- depth + 1L
#     if (depth == maxdepth | !is.recursive(x)) return(unlist(x, recursive  = FALSE)); 
#     lapply(x, thefun)
#  }
#   thefun
# }
# ifun <- mk_level2(2)
# ifun(st_geometry(nc[1, ]))
# 
# 
# ## multipolygon to multilinestring
# mk_level2(maxdepth = 2)(st_geometry(nc[57, ]))
# 
# as_list <- function(x, ...) {
#   UseMethod("as_list")
# }
# as_list.MULTIPOLYGON <- function(x, ...) {
#   ifun <- mk_level(maxdepth = 1L)
#   ifun(x)
# }
# as_list.sfc <- function(x, ...) {
#   lapply(x, as_list)
# }

