% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s2.R
\name{s2}
\alias{s2}
\alias{sf_use_s2}
\alias{st_as_s2}
\alias{st_as_s2.sf}
\alias{st_as_s2.sfc}
\title{functions for spherical geometry, using s2 package}
\usage{
sf_use_s2(use_s2)

st_as_s2(x, ...)

\method{st_as_s2}{sf}(x, ...)

\method{st_as_s2}{sfc}(
  x,
  ...,
  oriented = getOption("s2_oriented", FALSE) || isTRUE(attr(x, "oriented")),
  rebuild = FALSE
)
}
\arguments{
\item{use_s2}{logical; if \code{TRUE}, use the s2 spherical geometry package
for geographical coordinate operations}

\item{x}{object of class \code{sf}, \code{sfc} or \code{sfg}}

\item{...}{passed on}

\item{oriented}{logical; if \code{FALSE}, polygons that
cover more than half of the globe are inverted; if \code{TRUE}, no reversal
takes place and it is assumed that the inside of the polygon is to the
left of the polygon's path.}

\item{rebuild}{logical; call \link[s2]{s2_rebuild} on the geometry (think of this as a \code{st_make_valid} on the sphere)}
}
\value{
\code{sf_use_s2} returns the value of this variable before (re)setting it,
invisibly if \code{use_s2} is not missing.
}
\description{
functions for spherical geometry, using the s2 package based on the google s2geometry.io library
}
\details{
\code{st_as_s2} converts an \code{sf} POLYGON object into a form readable by \code{s2}.
}
\examples{
m = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
m1 = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,0), c(-1,-1))
m0 = m[5:1,]
mp = st_multipolygon(list(
list(m, 0.8 * m0, 0.01 * m1 + 0.9),
list(0.7* m, 0.6*m0),
list(0.5 * m0),
list(m+2),
list(m+4,(.9*m0)+4)
))
sf = st_sfc(mp, mp, crs = 'EPSG:4326')
s2 = st_as_s2(sf)
}
