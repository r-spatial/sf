/*
 everything with "write" is meant as "write from R into foreign (wkb)",
 "read" as "read from foreign (wkb) into R".
*/

#include <iostream>
#include <iomanip>
#include <cstdint>
#include <sstream>
#include <string>
#include <climits>

#include <math.h> // round()
#include <string.h> // memcpy()

#include <Rcpp.h>

#include "bbox.h"
#include "wkb.h"

/*      NULL/EMPTY             0 */
#define SF_Point               1
#define SF_LineString          2
#define SF_Polygon             3
#define SF_MultiPoint          4
#define SF_MultiLineString     5
#define SF_MultiPolygon        6
#define SF_GeometryCollection  7
#define SF_CircularString      8
#define SF_CompoundCurve       9
#define SF_CurvePolygon       10
#define SF_MultiCurve         11
#define SF_MultiSurface       12
#define SF_Curve              13
#define SF_Surface            14
#define SF_PolyhedralSurface  15
#define SF_TIN                16
#define SF_Triangle           17

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

Rcpp::List read_data(const unsigned char **pt, bool EWKB, int endian, bool addclass, 
	int *type, uint32_t *srid);
void write_data(std::ostringstream& os, Rcpp::List sfc, int i, bool EWKB, 
		int endian, const char *cls, const char *dim, double prec, int srid);

// https://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
template <typename T>
T swap_endian(T u) {
    static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");
    union {
        T u;
        unsigned char u8[sizeof(T)];
    } source, dest;
    source.u = u;
    for (size_t k = 0; k < sizeof(T); k++)
        dest.u8[k] = source.u8[sizeof(T) - k - 1];
    return dest.u;
}

inline unsigned char char2int(char c) {
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	throw std::range_error("char2int: false character in hex string");
}

// [[Rcpp::export]]
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx) {
// HexToRaw modified from cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors

// convert a hexadecimal string into a raw vector
// this version, dropping istringstream and std::hex, is 12 time faster than
// the one in the wkb github issue. C rules.

	Rcpp::List output(cx.size());
	for (int j = 0; j < cx.size(); j++) {
		Rcpp::RawVector raw(cx[j].size() / 2);
		const char *cp = cx[j];
		for (int i = 0; i < raw.size(); i++) {
			raw[i] = (char2int(cp[0]) << 4) + char2int(cp[1]);
			cp += 2;
			if (i % 100000 == 0)
				Rcpp::checkUserInterrupt();
		}
		output[j] = raw;
		if (j % 1000 == 0)
			Rcpp::checkUserInterrupt();
	}
	return output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_raw_to_hex(Rcpp::RawVector raw) {
	std::ostringstream os;
	char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f' };
	unsigned char *cp = &(raw[0]);
	for (int i = 0; i < raw.size(); i++) {
		int high = ((int) cp[i]) / 16;
		int low =  ((int) cp[i]) % 16;
  		os.write(&hex[high], sizeof(char));
  		os.write(&hex[low], sizeof(char));
	}
	return Rcpp::CharacterVector::create(os.str());
}

Rcpp::NumericMatrix read_multipoint(const unsigned char **pt, int n_dims, bool swap, 
		bool EWKB = 0, int endian = 0, Rcpp::CharacterVector cls = "", bool *empty = NULL) {
	uint32_t npts;
	memcpy(&npts, *pt, sizeof(uint32_t));
	if (swap)
		npts = swap_endian<uint32_t>(npts);
	(*pt) += 4;
	Rcpp::NumericMatrix ret(npts, n_dims);
	for (size_t i = 0; i < npts; i++) {
		Rcpp::List lst = read_data(pt, EWKB, endian, false, NULL, NULL);
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

Rcpp::List read_geometrycollection(const unsigned char **pt, int n_dims, bool swap, bool EWKB = 0, 
		int endian = 0, Rcpp::CharacterVector cls = "", bool isGC = true, bool *empty = NULL) {
	uint32_t nlst;
	memcpy(&nlst, *pt, sizeof(uint32_t));
	if (swap)
		nlst = swap_endian<uint32_t>(nlst);
	(*pt) += 4;
	Rcpp::List ret(nlst);
	for (size_t i = 0; i < nlst; i++)
		ret[i] = read_data(pt, EWKB, endian, isGC, NULL, NULL)[0];
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = nlst == 0;
	return ret;
}

Rcpp::NumericVector read_numeric_vector(const unsigned char **pt, int n, bool swap,
		Rcpp::CharacterVector cls = "") {
	Rcpp::NumericVector ret(n);
	for (int i = 0; i < n; i++) {
		double d;
		memcpy(&d, *pt, sizeof(double));
		if (swap)
			ret(i) = swap_endian<double>(d);
		else
			ret(i) = d;
		(*pt) += 8;
	}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	return ret;
}

Rcpp::NumericMatrix read_numeric_matrix(const unsigned char **pt, int n_dims, bool swap,
		Rcpp::CharacterVector cls = "", bool *empty = NULL) {
	uint32_t npts;
	memcpy(&npts, *pt, sizeof(uint32_t));
	if (swap)
		npts = swap_endian<uint32_t>(npts);
	(*pt) += 4;
	Rcpp::NumericMatrix ret(npts, n_dims);
	for (size_t i = 0; i < npts; i++)
		for (int j = 0; j< n_dims; j++) {
			double d;
			memcpy(&d, *pt, sizeof(double));
			if (swap)
				ret(i, j) = swap_endian<double>(d);
			else
				ret(i, j) = d;
			(*pt) += 8;
		}
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = npts == 0;
	return ret;
}

Rcpp::List read_matrix_list(const unsigned char **pt, int n_dims, bool swap, 
		Rcpp::CharacterVector cls = "", bool *empty = NULL) {
	uint32_t nlst;
	memcpy(&nlst, *pt, sizeof(uint32_t));
	if (swap)
		nlst = swap_endian<uint32_t>(nlst);
	(*pt) += 4;
	Rcpp::List ret(nlst);
	for (size_t i = 0; i < nlst; i++)
		ret[i] = read_numeric_matrix(pt, n_dims, swap, "");
	if (cls.size() == 3)
		ret.attr("class") = cls;
	if (empty != NULL)
		*empty = nlst == 0;
	return ret;
}

Rcpp::List read_data(const unsigned char **pt, bool EWKB = false, int endian = 0, 
		bool addclass = true, int *type = NULL, uint32_t *srid = NULL) {
/*
 pt: handle to the memory buffer
 EWKB: should we read EWKB, as opposed to ISO WKB?
 endian: 0 or 1, indicating big (0) or little (1) endian of the buffer
 addclass: write class information to object?
 type: IF NOT NULL: output the geometry type of object read
 srid: IF NOT NULL: output the srid read
 */

	Rcpp::List output(1); // to deal with varying result type
	bool swap = ((int) (**pt) != (int) endian); // endian check
	(*pt)++;
	// read type:
	uint32_t wkbType;
	memcpy(&wkbType, *pt, sizeof(uint32_t));
	if (swap)
		wkbType = swap_endian<uint32_t>(wkbType);
	(*pt) += 4;
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
				memcpy(srid, *pt, sizeof(uint32_t));
				if (swap)
					*srid = swap_endian<uint32_t>(*srid);
			}
			(*pt) += 4;
		}
	} else { // ISO
		sf_type = wkbType % 1000;
		switch (wkbType / 1000) { // 0: XY, 1: XYZ, 2: XYM, 3: XYZM
			case 0: n_dims = 2; dim_str = "XY"; break; 
			case 1: n_dims = 3; dim_str = "XYZ"; break; 
			case 2: n_dims = 3; dim_str = "XYM"; break; 
			case 3: n_dims = 4; dim_str = "XYZM"; break; 
			default:
				throw std::range_error("unknown wkbType dim in switch");
		}
	}
	bool empty = false;
	switch(sf_type) {
		case SF_Point: 
			output[0] = read_numeric_vector(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POINT", "sfg") : "");
			break;
		case SF_LineString:
			output[0] = read_numeric_matrix(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "LINESTRING", "sfg") : "", &empty);
			break;
		case SF_Polygon: 
			output[0] = read_matrix_list(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POLYGON", "sfg") : "", &empty);
			break;
		case SF_MultiPoint: 
			output[0] = read_multipoint(pt, n_dims, swap, EWKB, endian, addclass ?  
				Rcpp::CharacterVector::create(dim_str, "MULTIPOINT", "sfg") : "", &empty); 
			break;
		case SF_MultiLineString:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTILINESTRING", "sfg"), false, &empty);
			break;
		case SF_MultiPolygon:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTIPOLYGON", "sfg"), false, &empty);
			break;
		case SF_GeometryCollection: 
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "GEOMETRYCOLLECTION", "sfg"), true,
				&empty);
			break;
		case SF_CircularString:
			output[0] = read_numeric_matrix(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CIRCULARSTRING", "sfg") : "", &empty);
			break;
		case SF_CompoundCurve:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "COMPOUNDCURVE", "sfg"), true, &empty); 
			break;
		case SF_CurvePolygon:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "CURVEPOLYGON", "sfg"), true, &empty); 
			break;
		case SF_MultiCurve:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTICURVE", "sfg"), true, &empty);
			break;
		case SF_MultiSurface:
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTISURFACE", "sfg"), true, &empty);
			break;
		case SF_Curve:
			output[0] = read_numeric_matrix(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CURVE", "sfg") : "", &empty); 
			break;
		case SF_Surface: 
			output[0] = read_matrix_list(pt, n_dims, swap, addclass ?
				Rcpp::CharacterVector::create(dim_str, "SURFACE", "sfg") : "", &empty);
			break;
		case SF_PolyhedralSurface: 
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "POLYHEDRALSURFACE", "sfg"), false, &empty);
			break;
		case SF_TIN: 
			output[0] = read_geometrycollection(pt, n_dims, swap, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "TIN", "sfg"), false, &empty);
			break;
		case SF_Triangle:
			output[0] = read_matrix_list(pt, n_dims, swap,
				Rcpp::CharacterVector::create(dim_str, "TRIANGLE", "sfg"), &empty);
			break;
		default: {
			Rcpp::Rcout << "type is " << sf_type << std::endl;
			throw std::range_error("reading this sf type is not supported, please file an issue");
		}
	}
	if (type != NULL) {
		if (empty)
			*type = 0;
		else
			*type = sf_type;
	}
	return output;
}

// [[Rcpp::export]]
Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB = false, int endian = 0) {
	Rcpp::List output(wkb_list.size());

	int type = 0, last_type = 0, n_types = 0, n_empty = 0;

	uint32_t srid = 0;
	for (int i = 0; i < wkb_list.size(); i++) {
		Rcpp::checkUserInterrupt();
		Rcpp::RawVector raw = wkb_list[i];
		const unsigned char *pt = &(raw[0]);
		output[i] = read_data(&pt, EWKB, endian, true, &type, &srid)[0];
		if (type == 0)
			n_empty++;
		if (n_types <= 1 && type != last_type) {
			last_type = type;
			n_types++; // check if there's more than 1 type:
		}
	}
	output.attr("single_type") = n_types <= 1; // if 0, we have only empty geometrycollections
	output.attr("n_empty") = (int) n_empty;
	if (EWKB == true)
		output.attr("epsg") = (int) srid;
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
		type = SF_Curve;
	else if (strcmp(cls, "SURFACE") == 0)
		type = SF_Surface;
	else if (strcmp(cls, "POLYHEDRALSURFACE") == 0)
		type = SF_PolyhedralSurface;
	else if (strcmp(cls, "TIN") == 0)
		type = SF_TIN;
	else if (strcmp(cls, "TRIANGLE") == 0)
		type = SF_Triangle;
	else {
		Rcpp::Rcout << cls << " :";
		throw std::range_error("unknown type!");
	}
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
	if (precision < 0.0) { // float, 4-byte precision
		float f = d;
		return (double) f;
	}
	return round(d * precision) / precision;
}

void add_double(std::ostringstream& os, double d, double prec = 0.0) {
  d = make_precise(d, prec); // doubles are ALLWAYS coordinates
  const char *cp = (char *)&d;
  os.write((char*) cp, sizeof(double));
}

void write_vector(std::ostringstream& os, Rcpp::NumericVector vec, double prec) {
	for (int i = 0; i < vec.length(); i++)
		add_double(os, vec(i), prec);
}

void write_matrix(std::ostringstream& os, Rcpp::NumericMatrix mat, double prec) {
	add_int(os, mat.nrow());
	for (int i = 0; i < mat.nrow(); i++)
		for (int j = 0; j < mat.ncol(); j++)
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

// write single simple feature object as WKB to stream os
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
		case SF_Curve: write_matrix(os, sfc[i], prec);
			break;
		case SF_Surface: write_matrix_list(os, sfc[i], prec);
			break;
		case SF_PolyhedralSurface: write_multipolygon(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_TIN: write_triangles(os, sfc[i], EWKB, endian, prec);
			break;
		case SF_Triangle: write_matrix_list(os, sfc[i], prec);
			break;
		default: {
			Rcpp::Rcout << "type is " << sf_type << "\n";
			throw std::range_error("writing this sf type is not supported, please file an issue");
		}
	}
}

int native_endian(void) {
	const int one = 1;
	unsigned char *cp = (unsigned char *) &one;
	return (int) *cp;
}

// [[Rcpp::export]]
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB = false, int endian = 0, 
		Rcpp::CharacterVector dim = "XY", double precision = 0.0) {

	Rcpp::List output(sfc.size()); // with raw vectors
	Rcpp::CharacterVector cls_attr = sfc.attr("class");
	const char *cls = cls_attr[0], *dm = dim[0];

	// got the following from:
	// http://stackoverflow.com/questions/24744802/rcpp-how-to-check-if-any-attribute-is-null
	Rcpp::CharacterVector classes;
	SEXP sxp = sfc.attr("classes"); // how to check whether an attribute is present w/o using SEXP?
	if (! Rf_isNull(sxp)) {         // only sfc_GEOMETRY, the mixed bag, sets this
		classes = sfc.attr("classes");
		if (classes.size() != sfc.size())
			throw std::range_error("attr classes has wrong size: please file an issue");
	}

	Rcpp::List crs = sfc.attr("crs"); 
	int srid = crs(0);
	if (srid == NA_INTEGER)
		srid = 0; // non-zero now means: we have an srid

	for (int i = 0; i < sfc.size(); i++) {
		Rcpp::checkUserInterrupt();
		std::ostringstream os;
		if (! Rf_isNull(sxp))
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
