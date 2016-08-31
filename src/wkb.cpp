#include <iostream>
#include <cstdint>

#include <Rcpp.h>

// using namespace Rcpp;

Rcpp::NumericMatrix ReadMPoints(unsigned char **pt, int n_dims, bool EWKB, int endian, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::NumericVector ReadNumericVector(unsigned char **pt, int n, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::NumericMatrix ReadNumericMatrix(unsigned char **pt, int n_dims, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::List ReadMatrixList(unsigned char **pt, int n_dims, 
	Rcpp::CharacterVector cls, uint32_t *srid);
Rcpp::List ReadGC(unsigned char **pt, int n_dims, bool EWKB, int endian, 
	Rcpp::CharacterVector cls, bool addclass, uint32_t *srid);
Rcpp::List ReadData(unsigned char **pt, bool EWKB, int endian, bool debug, bool addclass);

// [[Rcpp::export]]
Rcpp::List HexToRaw(Rcpp::CharacterVector cx) {
// HexToRaw modified from cmhh, see https://github.com/ianmcook/wkb/issues/10
// @cmhh: if you make yourself known, I can add you to the contributors

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
Rcpp::List ReadWKB(Rcpp::List wkb_list, bool EWKB = false, int endian = 0, bool debug = false) {
	Rcpp::List output(wkb_list.size());
	for (int i = 0; i < wkb_list.size(); i++) {
		Rcpp::RawVector raw = wkb_list[i];
		unsigned char *pt = &(raw[0]);
		output[i] = ReadData(&pt, EWKB, endian, debug, true)[0];
	}
	return output;
}

Rcpp::List ReadData(unsigned char **pt, bool EWKB = false, int endian = 0, 
		bool debug = false, bool addclass = true) {

	Rcpp::List output(1);
	// do endian check, only support native endian WKB:
	if ((int) (**pt) != (int) endian)
		throw std::range_error("non native endian: use pureR = TRUE");
	(*pt)++;
	// read type:
	uint32_t *wkbType = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	uint32_t *srid = NULL;
	// Rprintf("[%u]\n", *wkbType);
	int sf_type = 0, dim = 0, n_dims = 0;
	std::string dim_str = ""; 
	if (EWKB) { // EWKB: PostGIS default
		sf_type =     *wkbType & 0x000000ff;
		int wkbZ =    *wkbType & 0x80000000;
		int wkbM =    *wkbType & 0x40000000;
		int wkbSRID = *wkbType & 0x20000000;
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
		case 1: 
			output[0] = ReadNumericVector(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POINT", "sfi") : "", srid);
			break;
		case 2:
			output[0] = ReadNumericMatrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "LINESTRING", "sfi") : "", srid); 
			break;
		case 3: 
			output[0] = ReadMatrixList(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "POLYGON", "sfi") : "", srid);
			break;
		case 4: 
			output[0] = ReadMPoints(pt, n_dims, EWKB, endian, addclass ?
				Rcpp::CharacterVector::create(dim_str, "MULTIPOINT", "sfi") : "", srid); 
			break;
		case 5:
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTILINESTRING", "sfi"), false, srid);
			break;
		case 6:
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTIPOLYGON", "sfi"), false, srid);
			break;
		case 7: 
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "GEOMETRYCOLLECTION", "sfi"), true, srid);
			break;
		case 8:
			output[0] = ReadNumericMatrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CIRCULARSTRING", "sfi") : "", srid); 
			break;
		case 11:
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTICURVE", "sfi"), false, srid);
			break;
		case 12:
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "MULTISURFACE", "sfi"), false, srid);
			break;
		case 13:
			output[0] = ReadNumericMatrix(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "CURVE", "sfi") : "", srid); 
			break;
		case 14: 
			output[0] = ReadMatrixList(pt, n_dims, addclass ?
				Rcpp::CharacterVector::create(dim_str, "SURFACE", "sfi") : "", srid);
			break;
		case 15: 
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "POLYHEDRALSURFACE", "sfi"), false, srid);
			break;
		case 16: 
			output[0] = ReadGC(pt, n_dims, EWKB, endian,
				Rcpp::CharacterVector::create(dim_str, "TIN", "sfi"), false, srid);
			break;
		case 17:
			output[0] = ReadMatrixList(pt, n_dims,
				Rcpp::CharacterVector::create(dim_str, "TRIANGLE", "sfi"), srid);
			break;
		default: 
			throw std::range_error("reading this sf type is not (yet) supported, please file an issue");
	}
	return(output);
}

Rcpp::NumericMatrix ReadMPoints(unsigned char **pt, int n_dims, bool EWKB = 0, int endian = 0, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	uint32_t *npts = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::NumericMatrix ret(*npts, n_dims);
	for (int i = 0; i < *npts; i++) {
		Rcpp::List lst = ReadData(pt, EWKB, endian);
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

Rcpp::List ReadGC(unsigned char **pt, int n_dims, bool EWKB = 0, int endian = 0, 
		Rcpp::CharacterVector cls = "", bool isGC = true, uint32_t *srid = NULL) {
	uint32_t *nlst = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::List ret(*nlst);
	for (int i = 0; i < *nlst; i++)
		ret[i] = ReadData(pt, EWKB, endian, false, isGC)[0];
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return(ret);
}

Rcpp::NumericVector ReadNumericVector(unsigned char **pt, int n, 
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

Rcpp::NumericMatrix ReadNumericMatrix(unsigned char **pt, int n_dims, 
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

Rcpp::List ReadMatrixList(unsigned char **pt, int n_dims, 
		Rcpp::CharacterVector cls = "", uint32_t *srid = NULL) {
	uint32_t *nlst = (uint32_t *) (*pt); // requires -std=c++11
	(*pt) += 4;
	Rcpp::List ret(*nlst);
	for (int i = 0; i < (*nlst); i++)
		ret[i] = ReadNumericMatrix(pt, n_dims, "");
	if (cls.size() == 3) {
		ret.attr("class") = cls;
		if (srid != NULL)
			ret.attr("epsg") = (int) *srid;
	}
	return(ret);
}

/*
    "Point",               # 1
    "LineString",          # 2
    "Polygon",             # 3
    "MultiPoint",          # 4
    "MultiLineString",     # 5
    "MultiPolygon",        # 6
    "GeometryCollection",  # 7
    "CircularString",      # 8 x
    "CompoundCurve",       # 9 x
    "CurvePolygon",        # 10 x
    "MultiCurve",          # 11 x
    "MultiSurface",        # 12 x
    "Curve",               # 13 x *
    "Surface",             # 14 x *
    "PolyhedralSurface",   # 15
    "TIN",                 # 16
    "Triangle"             # 17
*/
