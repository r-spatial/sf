#include <iostream>
#include <iomanip>
#include <cstdint>
#include <sstream>
#include <string>

#include <Rcpp.h>

#include "bbox.h"
#include "wkb.h"

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

Rcpp::NumericMatrix read_multipoint(unsigned char **pt, int n_dims, bool EWKB, int endian, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::NumericVector read_numeric_vector(unsigned char **pt, int n, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::NumericMatrix read_numeric_matrix(unsigned char **pt, int n_dims, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::List read_matrix_list(unsigned char **pt, int n_dims, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::List read_geometrycollection(unsigned char **pt, int n_dims, bool EWKB, int endian, 
	Rcpp::CharacterVector cls, bool addclass, uint32_t *srid);
Rcpp::List read_data(unsigned char **pt, bool EWKB, int endian, bool debug, 
	bool addclass, int *type);
void write_data(std::ostringstream& os, Rcpp::List sfc, int i, bool EWKB, int endian,
	bool debug, const char *cls, const char *dim, double precision);
unsigned int make_type(const char *cls, const char *dim, bool EWKB, int *tp);

// [[Rcpp::export]]
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx) {
// HexToRaw modified from cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors

// convert a hexadecimal string into a raw vector

	Rcpp::List output(cx.size());
	Rcpp::CharacterVector invec(cx);
	for (int j=0; j<cx.size(); j++) {
		Rcpp::RawVector raw(invec[j].size() / 2);
		std::string s = Rcpp::as<std::string>(invec[j]);
		int x;
		for (int i=0; i<raw.size(); i++){
			std::istringstream iss(s.substr(i*2, 2));
			iss >> std::hex >> x;
			raw[i] = x;
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
Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB = false, int endian = 0, 
		bool debug = false) {
	Rcpp::List output(wkb_list.size());

	int type = 0, last_type = 0, n_types = 0;

	for (int i = 0; i < wkb_list.size(); i++) {
		Rcpp::checkUserInterrupt();
		Rcpp::RawVector raw = wkb_list[i];
		unsigned char *pt = &(raw[0]);
		output[i] = read_data(&pt, EWKB, endian, debug, true, &type)[0];
		if (type != last_type) {
			last_type = type;
			n_types++;
		}
	}
	output.attr("n_types") = n_types; // if this is 1, we can skip the coerceTypes later on
	return output;
}

Rcpp::List read_data(unsigned char **pt, bool EWKB = false, int endian = 0, 
		bool debug = false, bool addclass = true, int *type = NULL) {

	Rcpp::List output(1); // to make result type opaque
	// do endian check, only support native endian WKB:
	if ((int) (**pt) != (int) endian)
		throw std::range_error("non native endian: use pureR = TRUE"); // life is too short
	(*pt)++;
	// read type:
	uint32_t *wkbType = (uint32_t *) (*pt); // uint32_t requires -std=c++11
	(*pt) += 4;
	uint32_t *srid = NULL;
	// Rprintf("[%u]\n", *wkbType);
	int sf_type = 0, dim = 0, n_dims = 0;
	std::string dim_str = ""; 
	if (EWKB) { // EWKB: PostGIS default
		sf_type =     *wkbType & 0x000000ff; // mask the other bits
		int wkbZ =    *wkbType & EWKB_Z_BIT;
		int wkbM =    *wkbType & EWKB_M_BIT;
		int wkbSRID = *wkbType & EWKB_SRID_BIT;
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
			srid = (uint32_t *) (*pt);
			(*pt) += 4;
		}
	} else { // ISO
		sf_type = *wkbType % 1000;
		switch (*wkbType / 1000) { // 0: XY, 1: XYZ, 2: XYM, 3: XYZM
			case 0: n_dims = 2; dim_str = "XY"; break; 
			case 1: n_dims = 3; dim_str = "XYZ"; break; 
			case 2: n_dims = 3; dim_str = "XYM"; break; 
			case 3: n_dims = 4; dim_str = "XYZM"; break; 
			default:
				throw std::range_error("unknown wkbType dim in switch");
		}
	}
	if (debug) {
		Rcpp::Rcout << "sf_type: " << sf_type << std::endl;
		Rcpp::Rcout << "n_dims:  " << n_dims << std::endl;
		Rcpp::Rcout << "dim_str: " << dim_str << std::endl;
		if (srid != NULL)
			Rcpp::Rcout << "srid: NA" << std::endl;
		else
			Rcpp::Rcout << "srid: " <<  *srid << std::endl;
	}
	switch(sf_type) {
		case SF_Point: 
			output[0] = read_numeric_vector(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POINT", "sfi") : "", srid);
			break;
		case SF_LineString:
			output[0] = read_numeric_matrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "LINESTRING", "sfi") : "", srid); 
			break;
		case SF_Polygon: 
			output[0] = read_matrix_list(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POLYGON", "sfi") : "", srid);
			break;
		case SF_MultiPoint: 
			output[0] = read_multipoint(pt, n_dims, EWKB, endian, addclass ?
				Rcpp::CharacterVector::create(dim_str, "MULTIPOINT", "sfi") : "", srid); 
			break;
		case SF_MultiLineString:
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTILINESTRING", "sfi"), false, srid);
			break;
		case SF_MultiPolygon:
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTIPOLYGON", "sfi"), false, srid);
			break;
		case SF_GeometryCollection: 
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "GEOMETRYCOLLECTION", "sfi"), true, srid);
			break;
		case SF_CircularString:
			output[0] = read_numeric_matrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CIRCULARSTRING", "sfi") : "", srid); 
			break;
		case SF_MultiCurve:
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTICURVE", "sfi"), false, srid);
			break;
		case SF_MultiSurface:
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTISURFACE", "sfi"), false, srid);
			break;
		case SF_Curve:
			output[0] = read_numeric_matrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CURVE", "sfi") : "", srid); 
			break;
		case SF_Surface: 
			output[0] = read_matrix_list(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "SURFACE", "sfi") : "", srid);
			break;
		case SF_PolyhedralSurface: 
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "POLYHEDRALSURFACE", "sfi"), false, srid);
			break;
		case SF_TIN: 
			output[0] = read_geometrycollection(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "TIN", "sfi"), false, srid);
			break;
		case SF_Triangle:
			output[0] = read_matrix_list(pt, n_dims,
				Rcpp::CharacterVector::create(dim_str, "TRIANGLE", "sfi"), srid);
			break;
		default: {
			Rcpp::Rcout << "type is " << sf_type << "\n";
			throw std::range_error("reading this sf type is not supported, please file an issue");
		}
	}
	if (type != NULL)
		*type = sf_type;
	return(output);
}

Rcpp::NumericMatrix read_multipoint(unsigned char **pt, int n_dims, bool EWKB = 0, int endian = 0, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	uint32_t *npts = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::NumericMatrix ret(*npts, n_dims);
	for (int i = 0; i < *npts; i++) {
		Rcpp::List lst = read_data(pt, EWKB, endian);
		Rcpp::NumericVector vec = lst[0];
		for (int j = 0; j < n_dims; j++)
			ret(i,j) = vec(j);
	}
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return(ret);
}

Rcpp::List read_geometrycollection(unsigned char **pt, int n_dims, bool EWKB = 0, int endian = 0, 
		Rcpp::CharacterVector cls = "", bool isGC = true, uint32_t *srid = NULL) {
	uint32_t *nlst = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::List ret(*nlst);
	for (int i = 0; i < *nlst; i++)
		ret[i] = read_data(pt, EWKB, endian, false, isGC)[0];
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return(ret);
}

Rcpp::NumericVector read_numeric_vector(unsigned char **pt, int n, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	Rcpp::NumericVector ret(n);
	double *d = (double *) (*pt);
	for (int i=0; i<n; i++)
		ret(i) = *d++;
	(*pt) = (unsigned char *) d;
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return ret;
}

Rcpp::NumericMatrix read_numeric_matrix(unsigned char **pt, int n_dims, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	uint32_t *npts = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::NumericMatrix ret(*npts, n_dims);
	double *d = (double *) (*pt);
	for (int i=0; i<(*npts); i++)
		for (int j=0; j<n_dims; j++)
			ret(i,j) = *d++;
	(*pt) = (unsigned char *) d;
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return ret;
}

Rcpp::List read_matrix_list(unsigned char **pt, int n_dims, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	uint32_t *nlst = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::List ret(*nlst);
	for (int i = 0; i < (*nlst); i++)
		ret[i] = read_numeric_matrix(pt, n_dims, "");
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return(ret);
}

//
// WriteWKB:
//

// [[Rcpp::export]]
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB = false, int endian = 0, 
		Rcpp::CharacterVector dim = "XY", bool debug = false, double precision = 0.0) {

	Rcpp::List output(sfc.size()); // with raw vectors
	int type = 0, last_type = 0, n_types = 0;
	Rcpp::CharacterVector cls_attr = sfc.attr("class");
	const char *cls = cls_attr[0], *dm = dim[0];
	if (debug) 
		Rcpp::Rcout << dm << std::endl;

	for (int i = 0; i < sfc.size(); i++) {
		std::ostringstream os;
		Rcpp::checkUserInterrupt();
		write_data(os, sfc, i, EWKB, endian, debug, cls, dm, precision);
		Rcpp::RawVector raw(os.str().size()); // os -> raw:
		std::string str = os.str();
		const char *cp = str.c_str();
		for (int j = 0; j < str.size(); j++)
			raw[j] = cp[j];
		output[i] = raw; // raw vector to list
	}
	return output;
}

unsigned int make_type(const char *cls, const char *dim, bool EWKB = false, int *tp = NULL) {
	int type = 0;
	if (strstr(cls, "sfc_") == cls)
		cls += 4;
	// Rcpp::Rcout << cls << " " << dim << std::endl;
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
	else
		throw std::range_error("unknown type!");
	*tp = type;
	if (EWKB) {
		if (strcmp(dim, "XYZ") == 0)
			type = type | EWKB_Z_BIT;
		else if (strcmp(dim, "XYM") == 0)
			type = type | EWKB_M_BIT;
		else if (strcmp(dim, "XYZM") == 0)
			type = type | EWKB_M_BIT | EWKB_Z_BIT;
	} else {
		if (strcmp(dim, "XYZ") == 0)
			type += 1000;
		else if (strcmp(dim, "XYM") == 0)
			type += 2000;
		else if (strcmp(dim, "XYZM") == 0)
			type += 3000;
	}
	return(type);
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
		return(d);
	if (precision < 0.0) { // float, 4-byte precision
		float f = d;
		return((double) f);
	}
	return(std::round(d * precision) / precision);
}

void add_double(std::ostringstream& os, double d, double prec = 0.0) {
  d = make_precise(d, prec);
  const char *cp = (char *)&d;
  os.write((char*) cp, sizeof(double));
}

void write_vector(std::ostringstream& os, Rcpp::NumericVector vec, double prec) {
	for (unsigned int i = 0; i < vec.length(); i++)
		add_double(os, vec(i), prec);
}

void write_matrix(std::ostringstream& os, Rcpp::NumericMatrix mat, double prec) {
	add_int(os, mat.nrow());
	for (unsigned int i = 0; i < mat.nrow(); i++)
		for (unsigned int j = 0; j < mat.ncol(); j++)
			add_double(os, mat(i,j), prec);
}

void write_matrix_list(std::ostringstream& os, Rcpp::List lst, double prec) {
	unsigned int len = lst.length();
	add_int(os, len);
	for (unsigned int i = 0; i < len; i++)
		write_matrix(os, lst[i], prec);
}

void write_multilinestring(std::ostringstream& os, Rcpp::List lst, bool EWKB = false, 
		int endian = 0, double prec = 0.0) {
	Rcpp::CharacterVector cl_attr = lst.attr("class");
	const char *dim = cl_attr[0];
	add_int(os, lst.length());
	for (int i = 0; i < lst.length(); i++)
		write_data(os, lst, i, EWKB, endian, false, "LINESTRING", dim, prec);
}

void write_multipolygon(std::ostringstream& os, Rcpp::List lst, bool EWKB = false, 
		int endian = 0, double prec = 0.0) {
	Rcpp::CharacterVector cl_attr = lst.attr("class");
	const char *dim = cl_attr[0];
	add_int(os, lst.length());
	for (int i = 0; i < lst.length(); i++)
		write_data(os, lst, i, EWKB, endian, false, "POLYGON", dim, prec);
}

void write_geometrycollection(std::ostringstream& os, Rcpp::List lst, bool EWKB = false, 
		int endian = 0, double prec = 0.0) {
	add_int(os, lst.length());
	Rcpp::Function Rclass("class");
	for (int i = 0; i < lst.length(); i++) {
		Rcpp::CharacterVector cl_attr = Rclass(lst[i]); 
		const char *cls = cl_attr[1], *dim = cl_attr[0];
		write_data(os, lst, i, EWKB, endian, false, cls, dim, prec);
	}
}

void write_multipoint(std::ostringstream& os, Rcpp::NumericMatrix mat, 
		bool EWKB = false, int endian = 0, double prec = 0.0) {
	add_int(os, mat.nrow());
	Rcpp::CharacterVector cl_attr = mat.attr("class");
	const char *dim = cl_attr[0];
	Rcpp::NumericVector v(mat.ncol()); // copy row i
	Rcpp::List lst(1);
	for (int i = 0; i < mat.nrow(); i++) {
		for (int j = 0; j < mat.ncol(); j++)
			v(j) = mat(i,j);
		lst[0] = v;
		write_data(os, lst, 0, EWKB, endian, false, "POINT", dim, prec);
	}
}

// write single simple feature object as WKB to stream os
void write_data(std::ostringstream& os, Rcpp::List sfc, int i = 0, bool EWKB = false, 
		int endian = 0, bool debug = false, const char *cls = NULL, const char *dim = NULL,
		double prec = 0.0) {
	
	add_byte(os, (char) endian);
	int tp;
	unsigned int sf_type = make_type(cls, dim, EWKB, &tp);
	if (debug)
		Rcpp::Rcout << "sf_type:" << sf_type << std::endl;
	add_int(os, sf_type);
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
		case SF_TIN: write_multipolygon(os, sfc[i], EWKB, endian, prec);
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
	// Rcpp::Rcout << "native endian: " << (int) *cp << std::endl;
	return((int) *cp);
}
