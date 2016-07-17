Simple features in R: design v. 2
=================================

Jul 16, 2016

Assumptions:
------------

1.  *simple features* refer to a set of 1 or more simple feature items
2.  simple feature items are composed of one or more points, or (nested) sets of those
3.  points have two components (xy), or three (xym or xyz), or
    four (xyzm)

Requirements/decisions:
-----------------------

1.  we restrict simple features to sets that share a common coordinate
    reference system \*
2.  we restrict simple features sets to those with a common type; mixed
    type can be encapsulated by GeometryCollections \*
3.  it shall be possible to store simple features as a variable
    (list column) in a data.frame, where each data.frame record relates
    to the corresponding feature; this column has bounding box and
    coordinate reference system as attributes
4.  simple features shall use simple R data structures (vector,
    matrix, list)
5.  simple features shall use class attributes (S3)
6.  subsetting simple features shall retain the coordinate reference
    system information
7.  a single point is represented as a numerical vector
8.  a set of points is represented by a matrix, each row representing a
    point
9.  a set of sets, or of pointsets, is represented by a list
10. for heterogeneous sets (GeometryCollection, CompoundCurve) the list
    shall be named

in this list, \* refers to properties shared by PostGIS tables

Simple represenations in R:
---------------------------

<table style="width:14%;">
<colgroup>
<col width="8%" />
<col width="5%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">WKT</th>
<th align="left">R</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Point(5 3)</td>
<td align="left">c(5,3)</td>
</tr>
<tr class="even">
<td align="left">LineString(5 3, 6 3, 6 4)</td>
<td align="left">matrix(c(5,3,6,3,6,4),ncol=2,byrow=TRUE)</td>
</tr>
<tr class="odd">
<td align="left">Polygon((5 3,6 3,6 4))</td>
<td align="left">list(matrix(c(5,3,6,3,6,4),ncol=2,byrow=TRUE))</td>
</tr>
<tr class="even">
<td align="left">Polygon((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))</td>
<td align="left">list(matrix(c(35,10, 45,45,15,40,10,20,35,10),ncol=2,byrow=TRUE), matrix(c(20,30,35,35,30,20,20,30),ncol=2,byrow=TRUE))</td>
</tr>
<tr class="odd">
<td align="left">MultiPoint(5 3,6 3,6 4)</td>
<td align="left">matrix(c(5,3,6,3,6,4),ncol=2,byrow=TRUE)</td>
</tr>
<tr class="even">
<td align="left">MultiLineString((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))</td>
<td align="left">list(matrix(c(35,10, 45,45,15,40,10,20,35,10),ncol=2,byrow=TRUE), matrix(c(20,30,35,35,30,20,20,30),ncol=2,byrow=TRUE))</td>
</tr>
<tr class="odd">
<td align="left">MultiPolygon(((5 3,6 3,6 4)), ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30)))</td>
<td align="left">list(list(matrix(c(5,3,6,3,6,4),ncol=2,byrow=TRUE)), list(matrix(c(35,10, 45,45,15,40,10,20,35,10),ncol=2,byrow=TRUE), matrix(c(20,30,35,35,30,20,20,30),ncol=2,byrow=TRUE)))</td>
</tr>
<tr class="even">
<td align="left">GeometryCollection(POINT(5 3),LINESTRING(5 3, 6 3, 6 4))</td>
<td align="left">list(point=c(5,3), linestring=matrix(c(5,3,6,3,6,4),ncol=2,byrow=TRUE)</td>
</tr>
<tr class="odd">
<td align="left">CircularString(1 5, 6 2, 7 3)</td>
<td align="left">matrix(c(1,5, 6,2, 7,3),ncol=2,byrow=TRUE)</td>
</tr>
<tr class="even">
<td align="left">CompoundCurve</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">CurvePolygon</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">MultiCurve</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">MultiSurface</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Curve</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Surface</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">PolyhedralSurface</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">TIN</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Triangle</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"><anything> EMPTY</td>
<td align="left">NULL</td>
</tr>
</tbody>
</table>

Todo:

    COMPOUNDCURVE(CIRCULARSTRING(0 0,1 1,1 0),(1 0,0 1))
    CURVEPOLYGON(CIRCULARSTRING(-2 0,-1 -1,0 0,1 -1,2 0,0 2,-2 0),(-1 0,0 0.5,1 0,0 1,-1 0))
    MULTICURVE((5 5,3 5,3 3,0 3),CIRCULARSTRING(0 0,2 1,2 2))
    TRIANGLE((0 0 0,0 1 0,1 1 0,0 0 0))
    TIN (((0 0 0, 0 0 1, 0 1 0, 0 0 0)), ((0 0 0, 0 1 0, 1 1 0, 0 0 0)))
    POLYHEDRALSURFACE Z (
        ((0 0 0, 0 1 0, 1 1 0, 1 0 0, 0 0 0)),
        ((0 0 0, 0 1 0, 0 1 1, 0 0 1, 0 0 0)),
        ((0 0 0, 1 0 0, 1 0 1, 0 0 1, 0 0 0)),
        ((1 1 1, 1 0 1, 0 0 1, 0 1 1, 1 1 1)),
        ((1 1 1, 1 0 1, 1 0 0, 1 1 0, 1 1 1)),
        ((1 1 1, 1 1 0, 0 1 0, 0 1 1, 1 1 1))
      )
