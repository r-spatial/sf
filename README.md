# sfr - simple features for R

Authors: Edzer Pebesma, Roger Bivand, ...

[Simple features](https://en.wikipedia.org/wiki/Simple_Features) (officially: _simple feature access_) is an open ([OGC](http://www.opengeospatial.org/standards/sfa) and [ISO](http://www.iso.org/iso/home/store/catalogue_tc/catalogue_detail.htm?csnumber=40114)) standard for access and manipulation of spatial vector data (points, lines, polygons). It includes a standard [SQL schema](http://www.opengeospatial.org/standards/sfs) that supports storage, retrieval, query and update of feature collections via a SQL interface. All commonly used databases provide this interface. [GeoJSON](http://geojson.org/) is a standard for encoding simple features in JSON, and is used in JavaScript and MongoDB. Well-known-text ([WKT](https://en.wikipedia.org/wiki/Well-known_text)) is a text representation of simple features used often in linked data; well-known-binary ([WKB] (https://en.wikipedia.org/wiki/Well-known_text)) a standard binary representation used in databases. _Simple Feature Access_ defines coordinate reference systems, and makes it easy to move data from longitude-latitude to projections back and forth in a standardized way. 


[GDAL](http://gdal.org/) is an open source C++ library for reading and writing both raster and vector data with more than 225 drivers (supported file formats, data base connectors, web service interfaces). GDAL is used by practically all open source geospatial projects and by many industry products (including ESRI's ArcGIS, ERDAS, and FME). It provides coordinate transformations (built on top of PROJ.4) and geometric operations (e.g. polygon intersections, unions, buffers and distance). Standards for coordinate transformations change over time; such changes are typically adopted directly in GDAL/PROJ.4 but do not easily find their way into R-only packages such as `mapproj`.

Since [2005](https://stat.ethz.ch/pipermail/r-sig-geo/2005-April/000378.html), CRAN has package [sp](https://cran.r-project.org/web/packages/sp/) which provides classes and methods for spatial (point, line, polygon and raster) data. The approach `sp` takes is similar to how `xts` and `zoo` handle the time index of time series data: objects store spatial geometries separately from associated attribute data, matching by order. Package [spacetime](https://cran.r-project.org/web/packages/spacetime/index.html), on CRAN since 2010, extends both `sp` and `xts` to handle data that varies over both space and time.

Today, 221 CRAN packages depend on, import or link to `sp`, 259 when including _Suggests_; when including recursive dependencies these numbers are 376 and 5040. The implementation of `sp` does not follow simple features, but rather the practice used at the time of release, following how ESRI shapefiles are implemented. The cluster of packages around `sp` is shown in [Andrie de Vries' blog](http://blog.revolutionanalytics.com/2015/07/the-network-structure-of-cran.html) in green.

Off-CRAN package [rgdal2](https://github.com/thk686/rgdal2) is an interface to GDAL 2.0, which uses raw pointers to interface features, but does not import any data in R, using GDAL to handle everything. CRAN Package [wkb](https://cran.r-project.org/web/packages/wkb/index.html), contributed by Tibco Software, converts between WKB representations of several simple feature classes and corresponding classes in `sp`, and seems to be needed for Tibco Software purposes.

<!---
[second edition](http://www.springer.com/statistics/life+sciences%2C+medicine+%26+health/book/978-1-4614-7617-7))
-->

## The problem
_What problem do you want to solve? Why is it a problem? Who does it affect? What will solving the problem enable? This section should include a brief summary of existing work, such as other packages. If you are proposing a change to R itself, you must include a letter of support from a member of R core._

The problems we want to solve are:

1. R can currently not represent simple features directly. It can read most simple feature classes in `sp` classes, but uses its own representation for this, and can only write data back without loss of information if it is furnished with ancilliary metadata encoded in a comment attribute to each Polygons object. It does for instance internally not distinguish between `POLYGON` and `MULTIPOLYGON`  nor deal with several simple feature classes, including `TIN` and `GEOMETRYCOLLECTION`, nor handle `CURVE` geometries.
2. The current implementation of lines and vector data in package `sp` is partly ambiguous (both slot `ringDir` or slot `hole` indicate whether a Polygon is a hole but are superceded by the comment attribute), complicated (to which exterior polygon does a hole belong - handled by the comment attribute), and by some considered difficult to work with (S4). The current implementation is hard to maintain because it contains incremental changes from a baseline that predated the industry-standard OGC SFS ([Open Geospatial Consortium](http://www.opengeospatial.org/) [Simple Feature Interface Specification](http://www.opengeospatial.org/standards/sfa)).
3. The lack of support for simple features makes current interfaces to open source libraries (GDAL/OGR and PROJ.4: rgdal, GEOS: rgeos) difficult to understand and maintain, even though they work to specification.
4. The current implementation has no scale model for coordinates.
5. It is desirable that other R packages are offered the opportunity to migrate to more up-to-date libraries for coordinate transformations (providing proper support for datum transformation), and to avoid having to make simplifying assumptions (e.g., all spatial data come as longitude/latitude using datum `WGS84`; all web maps use [_web Mercator_](https://en.wikipedia.org/wiki/Web_Mercator)).

Solving this problem will mainly affect those who use data bases or modern javascript-based web APIs, as these largely converged on adopting simple features, and those who need a simpler and more light-weight handling of spatial data in R. It will also reduce the effort for users and developers to understand the way spatial information is represented in R, make it easier to build upon and reuse the R code for this, and lead to a good, sustainable shared R code base.

In the longer run it will affect all packages currently using sp, when we manage to migrate sp to exclusively use the simple feature classes for representing vector data. Since the recent [2.0](http://www.gdal.org/index.html) release of GDAL integrates raster and vector data, having an R package that mirrors its classes makes it possible to implement operations in-database (similar to what `DBI`, `RPostgreSQL` and `dplyr` do), making it possible to work with data that do not fit in memory.

## The plan
_How are you going to solve the problem? Include the concrete actions you will take and an estimated timeline. What are likely failure modes and how will you recover from them?_

The plan is to 

1. develop an R package that implements simple features in R, that is simple yet gives users access to the complete data, and has a representation that uses S3 by extending `data.frame` (M1-3)
2. add to this package a C++ interface to GDAL 2.0, to read and write simple feature data, and to interface other functionality (coordinate transformation, geometry operations) (M3-8)
3. develop and prototypically implement a migration path for sp to become compliant with simple features (M7-12)
4. write user-oriented tutorial vignettes showing how to use it with files, data base connections, web API's, leaflet, ggmap, dplyr and so on (M7-10)
5. write a tutorial vignette for R package writers reusing the package (M10)
6. Collect and process community feed back (M11-12).

|  Failure mode    |  Recovery plan     |
|------------------|--------------------|
| S3 classes are too simple to represent simple features class hierarchy | try (i) using a list column with geometry, and nested lists to represent nested structures; (ii) use a `WKT` character column; (iii) using a `WKB` blob column | 
| CRAN does not (yet) support GDAL 2.0 | develop simple features in a package not depending on GDAL 2.0, and the GDAL 2.0 code in a second package; convince CRAN maintainers to update |
| migrating `sp` breaks downstream packages | discuss with Roger Bivand, Barry Rowlingson, Robert Hijmans (`raster`) and Tim Keitt (`rgdal`/`rgdal2`) how to proceed; be patient and try to smooth out problems |


## How can the ISC help
_What can we do to help you? If you need money, how much? How will you spend it? We expect that most of the budget will be on people, but we will  consider funding travel and equipment if you have good justification. How can we help promote your project? How else could we help?_

_Please note that the budget for the ISC is currently limited. We are likely to fund one or two projects with a budget of $20-30k projects, and handful of projects with budgets of  of $5-10k. We do not pay overhead._


The following table contains the cost items.

| Item | Cost |
| ---- | ---- |
| employ a student assistant for one year (10 hrs/week) | &euro; 6500  |
| invite Roger Bivand to visit the Institute for Geoinformatics for one week | &euro; 1000 |
| present the results at UseR! 2016 | &euro; 1500 |
| Total: | &euro; 9000 (9750 USD) |

The visit of Roger is anticipated halfway the project; further communications will use skype. The project has a planned duration of 12 months.

## Dissemination

_How will you ensure that your work is available to the widest number of people? What open source license will you use? How will host your code so that others can contribute? How will you publicise your work? We encourage you to plan at least two blog posts to the R consortium blog: once to announce the project, and one to write up what you achieved._


Development will take place on github, information will be shared and reactions and contributions invited through [r-sig-geo](https://stat.ethz.ch/mailman/listinfo/r-sig-geo), as well as [StackOverflow](http://stackoverflow.com/) and [GIS StackExchange](http://gis.stackexchange.com/). The project will use an Apache 2.0 license for maximum dissemination (similar to GDAL, which uses X/MIT). The work will be published in 4 blogs (quarterly), announced on r-sig-geo (3300 subscribers), and intermediary results will be presented at [UseR! 2016](http://user2016.org/). The final result will be published in a paper either submitted to [The R Journal](https://journal.r-project.org/) or to the [Journal of Statistical Software](http://www.jstatsoft.org/).
