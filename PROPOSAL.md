# Simple Features for R

Applicant: [Edzer Pebesma](https://github.com/edzer/), [Institute for Geoinformatics](https://www.uni-muenster.de/Geoinformatics/en/), University of Muenster, Germany; [edzer.pebesma@uni-muenster.de](mailto:edzer.pebesma@uni-muenster.de)

Supporting authors: Edzer Pebesma, Roger Bivand, Michael Sumner, Robert Hijmans, Virgilio Gómez-Rubio

[Simple features](https://en.wikipedia.org/wiki/Simple_Features) is an open ([OGC](https://www.ogc.org/standard/sfa/) and [ISO](https://www.iso.org/standard/40114.html)) interface standard for access and manipulation of spatial vector data (points, lines, polygons). It includes a standard [SQL schema](http://www.opengeospatial.org/standards/sfs) that supports storage, retrieval, query and update of feature collections via a SQL interface. All commonly used databases provide this interface. [GeoJSON](https://geojson.org/) is a standard for encoding simple features in JSON, and is used in JavaScript and MongoDB. Well-known-text ([WKT](https://en.wikipedia.org/wiki/Well-known_text)) is a text representation of simple features used often in linked data; well-known-binary ([WKB] (https://en.wikipedia.org/wiki/Well-known_text)) a standard binary representation used in databases. _Simple Feature Access_ defines coordinate reference systems, and makes it easy to move data from longitude-latitude to projections back and forth in a standardized way. 


[GDAL](https://gdal.org/) is an open source C++ library for reading and writing both raster and vector data with more than 225 drivers (supported file formats, data base connectors, web service interfaces). GDAL is used by practically all open source geospatial projects and by many industry products (including ESRI's ArcGIS, ERDAS, and FME). It provides coordinate transformations (built on top of PROJ.4) and geometric operations (e.g. polygon intersections, unions, buffers and distance). Standards for coordinate transformations change over time; such changes are typically adopted directly in GDAL/PROJ.4 but do not easily find their way into R-only packages such as `mapproj`.

Since [2005](https://stat.ethz.ch/pipermail/r-sig-geo/2005-April/000378.html), CRAN has package [sp](https://cran.r-project.org/package=sp) which provides classes and methods for spatial (point, line, polygon and raster) data. The approach `sp` takes is similar to how `xts` and `zoo` handle the time index of time series data: objects store spatial geometries separately from associated attribute data, matching by order. Package [spacetime](https://cran.r-project.org/package=spacetime), on CRAN since 2010, extends both `sp` and `xts` to handle data that varies over both space and time.

Today, 221 CRAN packages depend on, import or link to `sp`, 259 when including _Suggests_; when including recursive dependencies these numbers are 376 and 5040. The implementation of `sp` does not follow simple features, but rather the practice used at the time of release, following how ESRI shapefiles are implemented. The cluster of packages around `sp` is shown in Andrie de Vries' [blog on CRAN's network structure](https://blog.revolutionanalytics.com/2015/07/the-network-structure-of-cran.html) in green.

Off-CRAN package [rgdal2](https://github.com/thk686/rgdal2) is an interface to GDAL 2.0, which uses raw pointers to interface features, but does not import any data in R, using GDAL to handle everything. CRAN Package [wkb](https://cran.r-project.org/package=wkb), contributed by Tibco Software, converts between WKB representations of several simple feature classes and corresponding classes in `sp`, and seems to be needed for Tibco software purposes.

<!---
[second edition](http://www.springer.com/statistics/life+sciences%2C+medicine+%26+health/book/978-1-4614-7617-7))
-->

## The problem

The problems we will solve are:

1. R can currently not represent simple features directly. It can read most simple feature classes in `sp` classes, but uses its own representation for this, and can only write data back without loss of information if it is furnished with ancilliary metadata encoded in a comment attribute to each Polygons object. It does for instance internally not distinguish between `POLYGON` and `MULTIPOLYGON`  nor deal with several simple feature classes, including `TIN` and `GEOMETRYCOLLECTION`, nor handle `CURVE` geometries.
2. The current implementation of lines and vector data in package `sp` is partly ambiguous (both slot `ringDir` or slot `hole` indicate whether a Polygon is a hole but are superceded by the comment attribute), complicated (to which exterior polygon does a hole belong - handled by the comment attribute), and by some considered difficult to work with (S4). The current implementation is hard to maintain because it contains incremental changes from a baseline that predated the industry-standard OGC/ISO ([Simple Feature Interface Specification](https://www.ogc.org/standard/sfa/)).
3. The lack of support for simple features makes current interfaces to open source libraries (GDAL/OGR and PROJ.4: rgdal, GEOS: rgeos) difficult to understand and maintain, even though they work to specification.
4. The current implementation has no [scale model](https://libgeos.org/doxygen/classgeos_1_1geom_1_1PrecisionModel.html#details) for coordinates.
5. It is desirable that other R packages are offered the opportunity to migrate to more up-to-date libraries for coordinate transformations (providing proper support for datum transformation), and to avoid having to make simplifying assumptions (e.g., all spatial data come as longitude/latitude using datum `WGS84`; all web maps use [_web Mercator_](https://en.wikipedia.org/wiki/Web_Mercator)).

Which users will benefit from solving these problems?  It will mainly affect those who use data bases or modern javascript-based web APIs which largely converged on adopting simple features (such as [CartoDB](https://cartodb.com/)), as well as those who need a simpler and more light-weight handling of spatial data in R. It will also reduce the effort for users and developers to understand the way spatial information is represented in R, making it easier to build upon and reuse the R code for this, and lead to a good, sustainable shared R code base.

In the longer run it will affect users of all packages currently reusing `sp` classes, when we manage to migrate `sp` to exclusively use the simple feature classes for representing vector data. Since the recent [2.0](https://gdal.org/index.html) release of GDAL integrates raster and vector data, having an R package that mirrors its classes makes it possible to implement operations in-database (similar to what `DBI`, `RPostgreSQL` and `dplyr` do), making it possible for R to manipulate spatial data that do not fit in memory.

Big Data analysis with R often proceeds by connecting R to a database that holds the data. All commonly used commercial and open source databases store spatial point, line and polygon data in the form of simple features. Representing simple features in R will simplify big data analysis for spatial data.

## The plan

We want to solve the problem by carrying out the following steps (M1 refers to month 1):

1. develop an R package that implements simple features in R, that is simple yet gives users access to the complete data, and includes an S3 representation that extends `data.frame` (M1-3)
2. add to this package a C++ interface to GDAL 2.0, to read and write simple feature data, and to interface other functionality (coordinate transformation, geometry operations) (M3-8)
3. develop and prototypically implement a migration path for sp to become compliant with simple features (M7-12)
4. write user-oriented tutorial vignettes showing how to use it with files, data base connections, web API's, leaflet, ggmap, dplyr and so on (M7-10)
5. write a tutorial vignette for R package writers reusing the package (M10)
6. Collect and process community feed back (M6-12).

Failure modes and recovery plan:

1. Failure mode: S3 classes are too simple to represent simple
features class hierarchy. Recovery plan: try (i) using a list column
with geometry, and nested lists to represent nested structures; (ii) 
use a `WKT` character column; (iii) using a `WKB` blob column

2. Migrating `sp` breaks downstream packages. Recovery plan: involve
Roger Bivand, Barry Rowlingson, Robert Hijmans (`raster`)
and Tim Keitt (`rgdal`/`rgdal2`) how to proceed; be patient and
smooth out problems together with package maintainers.

## How can the ISC help

The following table contains the cost items.

| Item | Cost |
| ---- | ---- |
| employ a student assistant for one year (10 hrs/week) | &euro; 6500  |
| one week visit of Roger Bivand to the Inst. for Geoinformatics | &euro; 1000 |
| present the results at UseR! 2016 | &euro; 1500 |
| Total: | &euro; 9000 (9750 USD) |

The visit of Roger is anticipated halfway the project; further communications will use skype. The project has a planned duration of 12 months.

## Dissemination

Development will take place on github, information will be shared and reactions and contributions invited through [r-sig-geo](https://stat.ethz.ch/mailman/listinfo/r-sig-geo), as well as [StackOverflow](https://stackoverflow.com/) and [GIS StackExchange](https://gis.stackexchange.com/). The project will use an Apache 2.0 license for maximum dissemination (similar to GDAL, which uses X/MIT). The work will be published in 4 blogs (quarterly), announced on r-sig-geo (3300 subscribers), and intermediary results will be presented at [UseR! 2016](https://www.r-project.org/conferences/useR-2016/). The final result will be published in a paper either submitted to [The R Journal](https://journal.r-project.org/) or to the [Journal of Statistical Software](https://www.jstatsoft.org/index); this paper will be available before publication as a package vignette.

## UseR! slides

UseR! 2016 slides are found [here](https://pebesma.staff.ifgi.de/pebesma_sfr.pdf).
