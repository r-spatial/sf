/*
 everything with "write" is meant as "write from R into foreign (wkb)",
 "read" as "read from foreign (wkb) into R".
*/

#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>

#include <math.h> // round()
#include <string.h> // memcpy()

#include <Rcpp.h>

#include "wkb.h"

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

// [[Rcpp::interfaces(r, cpp)]]

typedef struct {
	const unsigned char *pt;
	size_t size;
} wkb_buf;

Rcpp::List read_data(wkb_buf *wkb, bool EWKB, bool spatialite, int endian,
	bool addclass, int *type, uint32_t *srid);
void write_data(std::ostringstream& os, Rcpp::List sfc, int i, bool EWKB,
		int endian, const char *cls, const char *dim, double prec, int srid);

static inline void wkb_read(wkb_buf *wkb, void *dst, size_t n) {
	if (n > wkb->size)
		Rcpp::stop("range check error: WKB buffer too small. Input file corrupt?"); // #nocov
	if (dst != NULL)
		memcpy(dst, wkb->pt, n);
	wkb->pt += n;
	wkb->size -= n;
}

template<typename T>
inline T wkb_read(wkb_buf *wkb) {
	if (sizeof(T) > wkb->size)
		Rcpp::stop("range check error: WKB buffer too small. Input file corrupt?");

	T dst;
	memcpy(&dst, wkb->pt, sizeof(T));
	wkb->pt += sizeof(T);
	wkb->size -= sizeof(T);
	return dst;
}

// https://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
template <typename T>
T swap_endian(T u) {
    union {
        T u;
        unsigned char u8[sizeof(T)];
    } source, dest;
    source.u = u;
    for (size_t k = 0; k < sizeof(T); k++)
        dest.u8[k] = source.u8[sizeof(T) - k - 1];
    return dest.u;
}


void read_spatialite_header(wkb_buf *wkb, uint32_t *srid, bool swap) {
	// we're at byte 3 now:
	*srid = wkb_read<uint32_t>(wkb);

	if (swap)
		*srid = swap_endian<uint32_t>(*srid); // #nocov

	wkb_read(wkb, NULL, 32); // skip header
	// verify special marker; if not there, raise error:
	unsigned char marker;
	wkb_read(wkb, &marker, 1); // skip header
	if (marker != 0x7c) {
		Rcpp::Rcout << "byte 39 should be 0x7c, but is " << marker << std::endl; // #nocov
		Rcpp::stop("invalid spatialite header"); // #nocov
	}
}

void read_gpkg_header(wkb_buf *wkb, uint32_t *srid, int endian) {
	// http://www.geopackage.org/spec/#gpb_format
	wkb_read(wkb, NULL, 3); // 'G', 'P', version

	// read flag:
	unsigned char flag;
	wkb_read(wkb, &flag, 1);
	bool swap = ((flag & 0x01) != (int) endian); // endian check

	// read srid, if needed, swap:
	*srid = wkb_read<uint32_t>(wkb);
	if (swap)
		*srid = swap_endian<uint32_t>(*srid); // #nocov

	// how much header is there to skip? bbox: 4, 6, 6, or 8 doubles:
	flag = (flag >> 1) & 0x07; // get bytes 3,2,1
	int n = 0;
	if (flag == 1) // [minx, maxx, miny, maxy]
		n = 32;
	else if (flag == 2 || flag == 3) // #nocov start
			// [minx, maxx, miny, maxy, minz, maxz] or [minx, maxx, miny, maxy, minm, maxm]
		n = 48;
	else if (flag == 4) // [minx, maxx, miny, maxy, minz, maxz, minm, maxm]
		n = 64; // #nocov end
	wkb_read(wkb, NULL, n);
}

Rcpp::NumericMatrix read_multipoint(wkb_buf *wkb, int n_dims, bool swap,
		bool EWKB = 0, bool spatialite = false, int endian = 0, Rcpp::CharacterVector cls = "",
		bool *empty = NULL) {

	uint32_t npts = wkb_read<uint32_t>(wkb);

	if (swap)
		npts = swap_endian<uint32_t>(npts); // #nocov

	Rcpp::NumericMatrix ret(npts, n_dims);
	for (size_t i = 0; i < npts; i++) {
		if (spatialite) {
			// verify special marker; if not there, raise error:
			unsigned char marker;
			wkb_read(wkb, &marker, 1); // absorb the 0x69 #nocov start
			if (marker != 0x69) {
				Rcpp::Rcout << "0x69 marker missing before ring " << i+1 << std::endl;
				Rcpp::stop("invalid spatialite header");
			} // #nocov end
		}
		Rcpp::List lst = read_data(wkb, EWKB, spatialite, endian, false, NULL, NULL);
		Rcpp::NumericVector vec = lst[0];
		for (int j = 0; j < n_dims; j++)
			ret(i,j) = vec(j);
	}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = npts == 0;
	return ret;
}

Rcpp::List read_geometrycollection(wkb_buf *wkb, int n_dims, bool swap, bool EWKB = 0,
		bool spatialite = false, int endian = 0, Rcpp::CharacterVector cls = "", bool isGC = true,
		bool *empty = NULL) {

	uint32_t nlst = wkb_read<uint32_t>(wkb);

	if (swap)
		nlst = swap_endian<uint32_t>(nlst); // #nocov

	Rcpp::List ret(nlst);

	for (size_t i = 0; i < nlst; i++) {
		if (spatialite) {
			// verify special marker; if not there, raise error
			unsigned char marker;
			wkb_read(wkb, &marker, 1); // absorb the 0x69
			if (marker != 0x69) { // #nocov start
				Rcpp::Rcout << "0x69 marker missing before ring " << i+1 << std::endl;
				Rcpp::stop("invalid spatialite header");
			} // #nocov end
		}
		ret[i] = read_data(wkb, EWKB, spatialite, endian, isGC, NULL, NULL)[0];
	}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = nlst == 0;
	return ret;
}

Rcpp::NumericVector read_numeric_vector(wkb_buf *wkb, int n, bool swap,
		Rcpp::CharacterVector cls = "", bool *empty = NULL) {
	Rcpp::NumericVector ret(n);
	for (int i = 0; i < n; i++) {
		double d = wkb_read<double>(wkb);
		if (swap)
			ret(i) = swap_endian<double>(d); // #nocov
		else
			ret(i) = d;
		if (i == 0 && empty != NULL && std::isnan(d))
			*empty = true;
	}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	return ret;
}

Rcpp::NumericMatrix read_numeric_matrix(wkb_buf *wkb, int n_dims, bool swap,
		Rcpp::CharacterVector cls = "", bool *empty = NULL) {

	uint32_t npts = wkb_read<uint32_t>(wkb);

	if (swap)
		npts = swap_endian<uint32_t>(npts); // #nocov

	Rcpp::NumericMatrix ret = Rcpp::no_init(npts, n_dims);
	for (size_t i = 0; i < npts; i++)
		for (int j = 0; j< n_dims; j++) {
			double d = wkb_read<double>(wkb);
			if (swap)
				ret(i, j) = swap_endian<double>(d); // #nocov
			else
				ret(i, j) = d;
		}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = npts == 0;
	return ret;
}

Rcpp::List read_matrix_list(wkb_buf *wkb, int n_dims, bool swap,
		Rcpp::CharacterVector cls = "", bool *empty = NULL) {

	uint32_t nlst = wkb_read<uint32_t>(wkb);

	if (swap)
		nlst = swap_endian<uint32_t>(nlst); // #nocov

	Rcpp::List ret(nlst);
	for (size_t i = 0; i < nlst; i++)
		ret[i] = read_numeric_matrix(wkb, n_dims, swap, "");

	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = nlst == 0;
	return ret;
}

Rcpp::List read_data(wkb_buf *wkb, bool EWKB = false, bool spatialite = false,
		int endian = 0, bool addclass = true, int *type = NULL, uint32_t *srid = NULL) {
/*
 pt: handle to the memory buffer
 EWKB: should we read EWKB, as opposed to ISO WKB?
 endian: 0 or 1, indicating big (0) or little (1) endian of the buffer
 addclass: write class information to object?
 type: IF NOT NULL: output the geometry type of object read
 srid: IF NOT NULL: output the srid read; NULL indicates a nested call
*/

	Rcpp::List output(1); // to deal with varying result type

	if (srid != NULL && wkb->size > 2 && wkb->pt[0] == 'G' && wkb->pt[1] == 'P') // GPKG header? skip:
		read_gpkg_header(wkb, srid, endian);

	if (spatialite && srid != NULL)
		wkb_read(wkb, NULL, 1); // starting 0x00 contains no information

	unsigned char swap_char;
	bool swap;

	if (spatialite && srid == NULL) // nested call: don't read swap:
		swap = false;
	else {
		wkb_read(wkb, &swap_char, 1);
		swap = ((int) swap_char != (int) endian); // endian check
	}
	if (spatialite) {
		if (swap)
			Rcpp::stop("reading non-native endian spatialite geometries not supported"); // #nocov
		if (srid != NULL) // not nested:
			read_spatialite_header(wkb, srid, swap);
	}

	// read type:
	uint32_t wkbType = wkb_read<uint32_t>(wkb);

	if (swap)
		wkbType = swap_endian<uint32_t>(wkbType); // #nocov

	int sf_type = 0, n_dims = 0;
	std::string dim_str = "";
	if (EWKB) { // EWKB: PostGIS default
		sf_type =     wkbType & 0x000000ff; // mask the other bits
		int wkbZ =    wkbType & EWKB_Z_BIT;
		int wkbM =    wkbType & EWKB_M_BIT;
		int wkbSRID = wkbType & EWKB_SRID_BIT;
		n_dims = 2 + (int) (wkbZ != 0) + (int) (wkbM != 0);
		if (wkbZ == 0 && wkbM == 0)
			dim_str = "XY";
		else if (wkbZ != 0 && wkbM == 0)
			dim_str = "XYZ";
		else if (wkbZ == 0 && wkbM != 1)
			dim_str = "XYM";
		else
			dim_str = "XYZM";
		if (wkbSRID != 0) {
			if (srid != NULL) {
				wkb_read(wkb, srid, 4);
				if (swap)
					*srid = swap_endian<uint32_t>(*srid); // #nocov
			}
		}
	} else { // ISO
		sf_type = wkbType % 1000;
		switch (wkbType / 1000) { // 0: XY, 1: XYZ, 2: XYM, 3: XYZM
			case 0: n_dims = 2; dim_str = "XY"; break;
			case 1: n_dims = 3; dim_str = "XYZ"; break;
			case 2: n_dims = 3; dim_str = "XYM"; break;
			case 3: n_dims = 4; dim_str = "XYZM"; break;
			default:
				Rcpp::Rcout << "wkbType: " << wkbType << std::endl; // #nocov
				Rcpp::stop("unsupported wkbType dim in switch"); // #nocov
		}
	}
	bool empty = false;
	switch(sf_type) {
		case SF_Point:
			output[0] = read_numeric_vector(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POINT", "sfg") : "", &empty);
			break;
		case SF_LineString:
			output[0] = read_numeric_matrix(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "LINESTRING", "sfg") : "", &empty);
			break;
		case SF_Polygon:
			output[0] = read_matrix_list(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POLYGON", "sfg") : "", &empty);
			break;
		case SF_MultiPoint:
			output[0] = read_multipoint(wkb, n_dims, swap, EWKB, spatialite, endian, addclass ?
				Rcpp::CharacterVector::create(dim_str, "MULTIPOINT", "sfg") : "", &empty);
			break;
		case SF_MultiLineString:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTILINESTRING", "sfg"), false, &empty);
			break;
		case SF_MultiPolygon:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTIPOLYGON", "sfg"), false, &empty);
			break;
		case SF_GeometryCollection:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "GEOMETRYCOLLECTION", "sfg"), true,
				&empty);
			break;
		case SF_CircularString:
			output[0] = read_numeric_matrix(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CIRCULARSTRING", "sfg") : "", &empty);
			break;
		case SF_CompoundCurve:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "COMPOUNDCURVE", "sfg"), true, &empty);
			break;
		case SF_CurvePolygon:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "CURVEPOLYGON", "sfg"), true, &empty);
			break;
		case SF_MultiCurve:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTICURVE", "sfg"), true, &empty);
			break;
		case SF_MultiSurface:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTISURFACE", "sfg"), true, &empty);
			break;
		case SF_Curve: // #nocov start
			output[0] = read_numeric_matrix(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CURVE", "sfg") : "", &empty);
			break;
		case SF_Surface:
			output[0] = read_matrix_list(wkb, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "SURFACE", "sfg") : "", &empty);
			break; // #nocov end
		case SF_PolyhedralSurface:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "POLYHEDRALSURFACE", "sfg"), false, &empty);
			break;
		case SF_TIN:
			output[0] = read_geometrycollection(wkb, n_dims, swap, EWKB, spatialite, endian,
				Rcpp::CharacterVector::create(dim_str, "TIN", "sfg"), false, &empty);
			break;
		case SF_Triangle:
			output[0] = read_matrix_list(wkb, n_dims, swap,
				Rcpp::CharacterVector::create(dim_str, "TRIANGLE", "sfg"), &empty);
			break;
		default: {
			char cp[100];
			snprintf(cp, 100, "reading wkb type %d is not supported\n", sf_type);
			Rcpp::stop(cp);
		}
	}
	if (type != NULL) {
		if (empty)
			*type = -sf_type;
		else
			*type = sf_type;
	}
	return output;
}

int native_endian(void) {
	const int one = 1;
	unsigned char *cp = (unsigned char *) &one;
	return (int) *cp;
}

// [[Rcpp::export]]
Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB = false, bool spatialite = false) {
	Rcpp::List output(wkb_list.size());

	int type = 0, last_type = 0, n_types = 0, n_empty = 0;
	int endian = native_endian();

	uint32_t srid = 0;
	for (int i = 0; i < wkb_list.size(); i++) {
		Rcpp::checkUserInterrupt();
		Rcpp::RawVector raw = wkb_list[i];
		wkb_buf wkb;
		wkb.pt = &(raw[0]);
		wkb.size = raw.size();
		// const unsigned char *pt = &(raw[0]);
		output[i] = read_data(&wkb, EWKB, spatialite, endian, true, &type, &srid)[0];
		if (type <= 0) {
			type = -type;
			n_empty++;
		}
		// Rcpp::Rcout << "type is " << type << "\n";
		if (n_types <= 1 && type != last_type) {
			last_type = type;
			n_types++; // check if there's more than 1 type:
		}
	}
	output.attr("single_type") = n_types <= 1; // if 0, we have only empty geometrycollections
	output.attr("n_empty") = (int) n_empty;
	if ((EWKB || spatialite) && srid != 0)
		output.attr("srid") = (int) srid;
	return output;
}

//
// write wkb:
//

unsigned int make_type(const char *cls, const char *dim, bool EWKB = false, int *tp = NULL,
		int srid = 0) {
	int type = 0;
	if (strstr(cls, "sfc_") == cls)
		cls += 4;
	if (strcmp(cls, "POINT") == 0)
		type = SF_Point;
	else if (strcmp(cls, "LINESTRING") == 0)
		type = SF_LineString;
	else if (strcmp(cls, "POLYGON") == 0)
		type = SF_Polygon;
	else if (strcmp(cls, "MULTIPOINT") == 0)
		type = SF_MultiPoint;
	else if (strcmp(cls, "MULTILINESTRING") == 0)
		type = SF_MultiLineString;
	else if (strcmp(cls, "MULTIPOLYGON") == 0)
		type = SF_MultiPolygon;
	else if (strcmp(cls, "GEOMETRYCOLLECTION") == 0)
		type = SF_GeometryCollection;
	else if (strcmp(cls, "CIRCULARSTRING") == 0)
		type = SF_CircularString;
	else if (strcmp(cls, "COMPOUNDCURVE") == 0)
		type = SF_CompoundCurve;
	else if (strcmp(cls, "CURVEPOLYGON") == 0)
		type = SF_CurvePolygon;
	else if (strcmp(cls, "MULTICURVE") == 0)
		type = SF_MultiCurve;
	else if (strcmp(cls, "MULTISURFACE") == 0)
		type = SF_MultiSurface;
	else if (strcmp(cls, "CURVE") == 0)
		type = SF_Curve; // #nocov
	else if (strcmp(cls, "SURFACE") == 0)
		type = SF_Surface; // #nocov
	else if (strcmp(cls, "POLYHEDRALSURFACE") == 0)
		type = SF_PolyhedralSurface;
	else if (strcmp(cls, "TIN") == 0)
		type = SF_TIN;
	else if (strcmp(cls, "TRIANGLE") == 0)
		type = SF_Triangle;
	else
		type = SF_Unknown; // a mix: GEOMETRY
	if (tp != NULL)
		*tp = type;
	if (EWKB) {
		if (strcmp(dim, "XYZ") == 0)
			type = type | EWKB_Z_BIT;
		else if (strcmp(dim, "XYM") == 0)
			type = type | EWKB_M_BIT;
		else if (strcmp(dim, "XYZM") == 0)
			type = type | EWKB_M_BIT | EWKB_Z_BIT;
		if (srid != 0)
			type = type | EWKB_SRID_BIT;
	} else {
		if (strcmp(dim, "XYZ") == 0)
			type += 1000;
		else if (strcmp(dim, "XYM") == 0)
			type += 2000;
		else if (strcmp(dim, "XYZM") == 0)
			type += 3000;
	}
	return type;
}

void add_byte(std::ostringstream& os, char c) {
  os.write((char*) &c, sizeof(char));
}

void add_int(std::ostringstream& os, unsigned int i) {
  const char *cp = (char *)&i;
  os.write((char*) cp, sizeof(int));
}

double make_precise(double d, double precision) {
	if (precision == 0.0)
		return d;
	if (precision < 0.0) { // round to float, 4-byte precision
		float f = d;
		return (double) f;
	}
	return round(d * precision) / precision;
}

void add_double(std::ostringstream& os, double d, double prec = 0.0) {
  d = make_precise(d, prec); // doubles are ALWAYS coordinates
  const char *cp = (char *)&d;
  os.write((char*) cp, sizeof(double));
}

void write_vector(std::ostringstream& os, Rcpp::NumericVector vec, double prec) {
	for (int i = 0; i < vec.length(); i++)
		add_double(os, vec(i), prec);
}

void write_matrix(std::ostringstream& os, Rcpp::NumericMatrix mat, double prec) {
	auto nrow = mat.nrow();
	auto ncol = mat.ncol();

	add_int(os, mat.nrow());
	for (decltype(nrow) i = 0; i < nrow; i++)
		for (decltype(ncol) j = 0; j < ncol; j++)
			add_double(os, mat(i,j), prec);
}

void write_matrix_list(std::ostringstream& os, Rcpp::List lst, double prec) {
	size_t len = lst.length();
	add_int(os, len);
	for (size_t i = 0; i < len; i++)
		write_matrix(os, lst[i], prec);
}

void write_multilinestring(std::ostringstream& os, Rcpp::List lst, bool EWKB = false,
		int endian = 0, double prec = 0.0) {
	Rcpp::CharacterVector cl_attr = lst.attr("class");
	const char *dim = cl_attr[0];
	add_int(os, lst.length());
	for (int i = 0; i < lst.length(); i++)
		write_data(os, lst, i, EWKB, endian, "LINESTRING", dim, prec, 0);
}

void write_multipolygon(std::ostringstream& os, Rcpp::List lst, bool EWKB = false,
		int endian = 0, double prec = 0.0) {
	Rcpp::CharacterVector cl_attr = lst.attr("class");
	const char *dim = cl_attr[0];
	add_int(os, lst.length());
	for (int i = 0; i < lst.length(); i++)
		write_data(os, lst, i, EWKB, endian, "POLYGON", dim, prec, 0);
}

void write_triangles(std::ostringstream& os, Rcpp::List lst, bool EWKB = false,
		int endian = 0, double prec = 0.0) {
	Rcpp::CharacterVector cl_attr = lst.attr("class");
	const char *dim = cl_attr[0];
	add_int(os, lst.length());
	for (int i = 0; i < lst.length(); i++)
		write_data(os, lst, i, EWKB, endian, "TRIANGLE", dim, prec, 0);
}

void write_geometrycollection(std::ostringstream& os, Rcpp::List lst, bool EWKB = false,
		int endian = 0, double prec = 0.0) {
	add_int(os, lst.length());
	Rcpp::Function Rclass("class");
	for (int i = 0; i < lst.length(); i++) {
		Rcpp::CharacterVector cl_attr = Rclass(lst[i]);
		const char *cls = cl_attr[1], *dim = cl_attr[0];
		write_data(os, lst, i, EWKB, endian, cls, dim, prec, 0);
	}
}

void write_multipoint(std::ostringstream& os, Rcpp::NumericMatrix mat,
		bool EWKB = false, int endian = 0, double prec = 0.0) {
	add_int(os, mat.nrow());
	Rcpp::CharacterVector cl_attr = mat.attr("class");
	const char *dim = cl_attr[0];
	Rcpp::NumericVector v(mat.ncol()); // copy row i
	for (int i = 0; i < mat.nrow(); i++) {
		for (int j = 0; j < mat.ncol(); j++)
			v(j) = mat(i,j);
		write_data(os, Rcpp::List::create(v), 0, EWKB, endian, "POINT", dim, prec, 0);
	}
}

// write single simple feature object as (E)WKB to stream os
void write_data(std::ostringstream& os, Rcpp::List sfc, int i = 0, bool EWKB = false,
		int endian = 0, const char *cls = NULL, const char *dim = NULL, double prec = 0.0,
		int srid = 0) {

	add_byte(os, (char) endian);
	int tp;
	unsigned int sf_type = make_type(cls, dim, EWKB, &tp, srid);
	add_int(os, sf_type);
	if (EWKB && srid != 0)
		add_int(os, srid);
	switch(tp) {
		case SF_Point: write_vector(os, sfc[i], prec);
			break;
		case SF_LineString: write_matrix(os, sfc[i], prec);
			break;
		case SF_Polygon: write_matrix_list(os, sfc[i], prec);
			break;
		case SF_MultiPoint: write_multipoint(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_MultiLineString: write_multilinestring(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_MultiPolygon: write_multipolygon(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_GeometryCollection: write_geometrycollection(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_CircularString: write_matrix(os, sfc[i], prec);
			break;
		case SF_CompoundCurve: write_geometrycollection(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_CurvePolygon: write_geometrycollection(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_MultiCurve: write_geometrycollection(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_MultiSurface: write_geometrycollection(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_Curve: write_matrix(os, sfc[i], prec); // #nocov start
			break;
		case SF_Surface: write_matrix_list(os, sfc[i], prec);
			break; // #nocov end
		case SF_PolyhedralSurface: write_multipolygon(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_TIN: write_triangles(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_Triangle: write_matrix_list(os, sfc[i], prec);
			break;
		default: {
			Rcpp::Rcout << "type is " << sf_type << "\n"; // #nocov
			Rcpp::stop("writing this sf type is not supported, please file an issue"); // #nocov
		}
	}
}

// [[Rcpp::export]]
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB = false) {

	double precision = sfc.attr("precision");
	Rcpp::CharacterVector cls_attr = sfc.attr("class");
	Rcpp::List sfc_dim = get_dim_sfc(sfc);
	Rcpp::CharacterVector dim = sfc_dim["_cls"];
	const char *cls = cls_attr[0], *dm = dim[0];

	Rcpp::List output(sfc.size()); // with raw vectors

	int endian = native_endian();

	// got the following from:
	// http://stackoverflow.com/questions/24744802/rcpp-how-to-check-if-any-attribute-is-null
	Rcpp::CharacterVector classes;
	bool have_classes = false;
	if (sfc.size() > 0 && strcmp(cls, "sfc_GEOMETRY") == 0) {
		if (sfc.hasAttribute("classes")) { // only sfc_GEOMETRY, the mixed bag, sets the classes attr
			classes = sfc.attr("classes");
			if (classes.size() != sfc.size())
				Rcpp::stop("attr classes has wrong size: please file an issue"); // #nocov
			have_classes = true;
		} else
			Rcpp::stop("sfc_GEOMETRY has no classes attribute; please file an issue"); // #nocov
	}

	int srid = 0;
	if (EWKB) { 
		// get SRID from crs[["input"]], either of the form "4326" 
		// or "XXXX:4326" with arbitrary XXXX string,
		// or else from the wkt field of the crs using srid_from_crs()
		Rcpp::List crs = sfc.attr("crs");
		Rcpp::CharacterVector input = crs(0);
		char *inp = input[0];
		char *remainder = NULL;
		// check for ":", and move one beyond:
		if ((remainder = strstr(inp, ":")) != NULL)
			inp = remainder + 1;
		long value = strtol(inp, &remainder, 10);
		if (*remainder == '\0') // strtol() succeeded:
			srid = (int) value;
		else {
			int i = srid_from_crs(crs);
			if (i != NA_INTEGER)
				srid = i; // else leave 0
		}
	}

	for (int i = 0; i < sfc.size(); i++) {
		Rcpp::checkUserInterrupt();
		std::ostringstream os;
		if (have_classes)
			cls = classes[i];
		write_data(os, sfc, i, EWKB, endian, cls, dm, precision, srid);
		Rcpp::RawVector raw(os.str().size()); // os -> raw:
		std::string str = os.str();
		const char *cp = str.c_str();
		for (size_t j = 0; j < str.size(); j++)
			raw[j] = cp[j];
		output[i] = raw; // raw vector to list
	}
	return output;
}

// get dim, "XY", "XYZ", "XYZM" or "XYM" from an sfc object

Rcpp::List get_dim_sfc(Rcpp::List sfc) {

	if (sfc.length() == 0)
		return Rcpp::List::create(
			Rcpp::Named("_cls") = Rcpp::CharacterVector::create("XY"),
			Rcpp::Named("_dim") = Rcpp::IntegerVector::create(2)
		);

	// we have data:
	Rcpp::CharacterVector cls = sfc.attr("class");
	unsigned int tp = make_type(cls[0], "", false, NULL, 0);
	if (tp == SF_Unknown) {
		cls = sfc.attr("classes");
		tp = make_type(cls[0], "", false, NULL, 0);
	}
	switch (tp) {
		case SF_Unknown: { // further check:
			Rcpp::stop("impossible classs in get_dim_sfc()"); // #nocov
		} break;
		case SF_Point: { // numeric:
			Rcpp::NumericVector v = sfc[0];
			cls = v.attr("class");
		} break;
		case SF_LineString:  // matrix:
		case SF_MultiPoint:
		case SF_CircularString:
		case SF_Curve: {
			Rcpp::NumericMatrix m = sfc[0];
			cls = m.attr("class");
		} break;
		case SF_Polygon: // list:
		case SF_MultiLineString:
		case SF_MultiPolygon:
		case SF_GeometryCollection:
		case SF_CompoundCurve:
		case SF_CurvePolygon:
		case SF_MultiCurve:
		case SF_MultiSurface:
		case SF_Surface:
		case SF_PolyhedralSurface:
		case SF_TIN:
		case SF_Triangle: {
			Rcpp::List l = sfc[0];
			cls = l.attr("class");
		} break;
	}

	return Rcpp::List::create(
		Rcpp::Named("_cls") = cls,
		Rcpp::Named("_dim") = strstr(cls[0], "Z") != NULL ?
			Rcpp::IntegerVector::create(3) :
			Rcpp::IntegerVector::create(2));
}

