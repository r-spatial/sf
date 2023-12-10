#' North Carolina SIDS data
#'
#' Sudden Infant Death Syndrome (SIDS) sample data for North Carolina counties,
#' two time periods (1974-78 and 1979-84). The details of the columns can be
#' found on the seealso URL, spdep package's vignette. Please note that,
#' though this is basically the same as \code{nc.sids} dataset in spData
#' package, \code{nc} only contains a subset of variables. The differences are
#' also discussed on the vignette.
#' @format A `sf` object 
#' @name nc
#' @docType data
#' @seealso \url{https://r-spatial.github.io/spdep/articles/sids.html}
#' @examples
#' \donttest{
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' }
NULL
