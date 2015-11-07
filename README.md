# sfr - simple features for R

[Simple features](https://en.wikipedia.org/wiki/Simple_Features) (oficially: _simple feature access_) is an open ([OGC](http://www.opengeospatial.org/standards/sfa) and [ISO](http://www.iso.org/iso/home/store/catalogue_tc/catalogue_detail.htm?csnumber=40114)) standard for access and manipulation of spatial vector data (points, lines, polygons). It includes a standard [SQL schema](http://www.opengeospatial.org/standards/sfs) that supports storage, retrieval, query and update of feature collections via a SQL interface. All commonly used databases provide this interface. [GeoJSON](http://geojson.org/) is a standard for encoding simple features in JSON, and is used in javascript and MongoDB. Well-known-text ([WKT](https://en.wikipedia.org/wiki/Well-known_text)) is a text representation of simple features used often in linked data; well-known-binary a standard binary representation used in databases. _Simple Feature Access_ defines coordinate reference systems, and makes it easy to move data from longitude-latitude to projections back and forth in a standardized way. 


[GDAL](http://gdal.org/) is an open source C++ library for reading and writing both raster and vector data with 224 drivers (supported file formats, data base connectors, web service interfaces). GDAL is used by practically all open source geospatial projects and by many industry products (including ESRI's ArcGIS, ERDAS, and FME). It provides coordinate transformations (built on top of PROJ.4) and geometric operations (e.g. polygon intersections, unions, buffers and distance). Standards for coordinates transformations change over time; such changes are typically adopted directly in GDAL/PROJ.4 but do not easily find there way in R-only packages such as `mapproj`.

Since [2005](https://stat.ethz.ch/pipermail/r-sig-geo/2005-April/000378.html), CRAN has package [sp](https://cran.r-project.org/web/packages/sp/) for dealing with point, line, polygon and raster data. Today, 221 CRAN packages depend on, import or link to sp, 376 when counting recursive dependencies, 259 non-recursive when including _Suggests_. The implementation of sp does not follow simple features, but rather the practice (and open source libraries) used back then, essentially based on how ESRI shapefiles are implemented.

<!---
[second edition](http://www.springer.com/statistics/life+sciences%2C+medicine+%26+health/book/978-1-4614-7617-7))
-->

## The problem
_What problem do you want to solve? Why is it a problem? Who does it affect? What will solving the problem enable? This section should include a brief summary of existing work, such as other packages. If you are proposing a change to R itself, you must include a letter of support from a member of R core._

The problems we want to solve are 

1. R can currently not represent simple features. It can read most simple feature classes, but has its own representation for this, and cannot write them back without loss of information (as it does for instance internally not distinguish between `POLYGON` and `MULTIPOLYGON`, and cannot deal with `FEATURECOLLECTION`).
2. The current implementation of lines and vector data in package `sp` is partly ambiguous (does slot `ringDir` or slot `hole` indicate whether a polygon is a hole?), complicated (to which exterior polygon does a hole belong?), and by some considered difficult to work with (S4).
3. The lack of simple features makes current interfaces to open source libraries (GDAL/OGR and PROJ.4: rgdal, GEOS: rgeos) difficult to understand and maintain
4. Several current approaches (e.g. `ggmap`, `ggplot`) tend to use non-standardised, R-only, and partly outdated libraries for coordinate transformations, or tend to make overly simplifying assumptions (spatial data come as longitude/latitude and have datum `WGS84`).

Solving this problem will mainly affect those who use data bases or modern javascript-based web APIs, as these largely converged on adopting simple features, and those who need a simpler and more light-weight handling of spatial data in R. It will also reduce the effort for users and developers to understand the way spatial information is represented in R, make it easier to build upon and reuse the R code for this, and lead to a good, sustainable shared R code base.

On the longer run it will affect all packages currently using sp, when we manage to migrate sp to exclusively use the simple feature classes. Since the recent [2.0](http://www.gdal.org/index.html) release of GDAL integrates raster and vector data, having an R package that mirrors its classes makes it possible to implement operations in-database (similar to what `DBI`, `RPostgreSQL` and `dplyr` do), making it possible to work with data that do not fit in memory.

## The plan
_How are you going to solve the problem? Include the concrete actions you will take and an estimated timeline. What are likely failure modes and how will you recover from them?_

The plan is to 

1. implement simple features in R, including a representation that uses S3 and extends `data.frame` (M1-3).
2. develop a C++ interface to GDAL 2.0, to read and write simple feature objects, and to interface other functionality (coordinate transformation, geometry operations) (M3-8)
3. develop vignettes with data base connections, web API's, leaflet, ggmap, dplyr (M7-10)
4. develop and prototypically implement a migration path for sp to become compliant with simple features. (M10-12)

|  Failure mode    |  Recovery plan     |
|------------------|--------------------|
| S3 classes are too simple to represent simple features | use a list column with geometry, and nested lists to represent nested structures | 
| CRAN does not (yet) support GDAL 2.0 | ask Roger Bivand for help, convince Uwe Ligges and Simon Urbanek |
| migrating sp break downstream packages | discuss with Roger Bivand, Barry Rowlingson, Robert Hijmans and Tim Keitt how to proceed; be patient and try to smooth out problems |



## How can the ISC help
_What can we do to help you? If you need money, how much? How will you spend it? We expect that most of the budget will be on people, but we will  consider funding travel and equipment if you have good justification. How can we help promote your project? How else could we help?_

_Please note that the budget for the ISC is currently limited. We are likely to fund one or two projects with a budget of $20-30k projects, and handful of projects with budgets of  of $5-10k. We do not pay overhead._


I need to employ a student assistant for one year (10 hrs/week),
which costs euro 6500, and 1000 euro for travel and subsistance
when Roger Bivand will visit us for one week, halfway the project.
There are no overheads involved in these costs.

## Dissemination

_How will you ensure that your work is available to the widest number of people? What open source license will you use? How will host your code so that others can contribute? How will you publicise your work? We encourage you to plan at least two blog posts to the R consortium blog: once to announce the project, and one to write up what you achieved._


Development will take place on github, information will be shared and reactions and contributions invited through r-sig-geo as well as StackOverflow. The project will use an Apache 2.0 license, for maximum dissemination (similar to GDAL, which uses X/MIT). The work will be published in 4 blogs (quarterly), as well as announced on r-sig-geo (3200 subscribers). The final result will be published in a paper either submitted to The R Journal or to the Journal of Statistical Software.
