% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfc.R
\name{sfc}
\alias{sfc}
\alias{st_sfc}
\alias{sfc_POINT}
\alias{sfc_LINESTRING}
\alias{sfc_POLYGON}
\alias{sfc_MULTIPOINT}
\alias{sfc_MULTILINESTRING}
\alias{sfc_MULTIPOLYGON}
\alias{sfc_GEOMETRYCOLLECTION}
\alias{[.sfc}
\title{Create simple feature geometry list column}
\usage{
st_sfc(
  ...,
  crs = NA_crs_,
  precision = 0,
  check_ring_dir = FALSE,
  dim,
  recompute_bbox = FALSE,
  oriented = NA
)

\method{[}{sfc}(x, i, j, ..., op = st_intersects)
}
\arguments{
\item{...}{zero or more simple feature geometries (objects of class \code{sfg}), or a single list of such objects; \code{NULL} values will get replaced by empty geometries.}

\item{crs}{coordinate reference system: integer with the EPSG code, or character with proj4string}

\item{precision}{numeric; see \link{st_as_binary}}

\item{check_ring_dir}{see \link{st_read}}

\item{dim}{character; if this function is called without valid geometries, this argument may carry the right dimension to set empty geometries}

\item{recompute_bbox}{logical; use \code{TRUE} to force recomputation of the bounding box}

\item{oriented}{logical; if \code{TRUE}, the ring is oriented such that left of the edges is inside the polygon; this is
needed for convering polygons larger than half the globe to s2}

\item{x}{object of class \code{sfc}}

\item{i}{record selection. Might also be an \code{sfc}/\code{sf} object to work with the \code{op} argument}

\item{j}{ignored if \code{op} is specified}

\item{op}{function, geometrical binary predicate function to apply when
\code{i} is a \code{sf}/\code{sfc} object. Additional arguments can be
specified using \code{...}, see examples.}
}
\value{
an object of class \code{sfc}, which is a classed list-column with simple feature geometries.
}
\description{
Create simple feature geometry list column, set class, and add coordinate reference system and precision.
For data.frame alternatives see \code{\link[=st_sf]{st_sf()}}. To convert a foreign object to \code{sfc}, see \code{\link[=st_as_sfc]{st_as_sfc()}}
}
\details{
A simple feature geometry list-column is a list of class
\code{c("stc_TYPE", "sfc")} which most often contains objects of identical type;
in case of a mix of types or an empty set, \code{TYPE} is set to the
superclass \code{GEOMETRY}.

if \code{x} has a \code{dim} attribute (i.e. is an \code{array} or \code{matrix}) then \code{op} cannot be used.
}
\examples{
pt1 = st_point(c(0,1))
pt2 = st_point(c(1,1))
(sfc = st_sfc(pt1, pt2))
sfc[sfc[1], op = st_is_within_distance, dist = 0.5]
d = st_sf(data.frame(a=1:2, geom=sfc))
}
