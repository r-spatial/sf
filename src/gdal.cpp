#include <gdal.h>
#include <gdal_version.h>
#include <gdal_alg.h>
#include <gdal_priv.h> // GDALDriver
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>
#include <ogr_spatialref.h>
#include <cpl_conv.h>
#include <cpl_string.h>

#include <stdlib.h> // atoi
#include <string.h>

#include <Rcpp.h>

#include "gdal.h"
#include "wkb.h"
#include "gdal_sf_pkg.h"

// global variable:
bool axis_order_authority_compliant = false;

//
// Returns errors to R
// Note only case 4 actually returns immediately
// Lower error codes are recoverable
//
static void __err_handler(CPLErr eErrClass, int err_no, const char *msg)
{
	switch ( eErrClass )
	{
        case 0:
            break; // #nocov
        case 1:
        case 2:
            Rf_warning("GDAL Message %d: %s\n", err_no, msg); // #nocov
            break; // #nocov
        case 3:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            break;
        case 4:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg); // #nocov
            Rcpp::stop("Unrecoverable GDAL error\n"); // #nocov
            break;
        default:
            Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg); // #nocov
            break; // #nocov
    }
    return;
}

// #nocov start
static void __err_silent(CPLErr eErrClass, int err_no, const char *msg)
{
    return;
}
// #nocov end

void set_error_handler(void)
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
}

void unset_error_handler(void)
{
    CPLSetErrorHandler((CPLErrorHandler)__err_silent);
}

// [[Rcpp::export]]
void CPL_gdal_init()
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
    GDALAllRegister();
    OGRRegisterAll();
}

// #nocov start
// [[Rcpp::export]]
void CPL_gdal_cleanup_all()
{
    OGRCleanupAll();
    OSRCleanup();
}
// #nocov end

// [[Rcpp::export]]
const char* CPL_gdal_version(const char* what = "RELEASE_NAME")
{
	return GDALVersionInfo(what);
}

void handle_error(OGRErr err) {
	if (err != OGRERR_NONE) {
		switch (err) {
			case OGRERR_NOT_ENOUGH_DATA:
				Rcpp::Rcout << "OGR: Not enough data " << std::endl; // #nocov
				break; // #nocov
			case OGRERR_UNSUPPORTED_GEOMETRY_TYPE:
				Rcpp::Rcout << "OGR: Unsupported geometry type" << std::endl;
				break;
			case OGRERR_CORRUPT_DATA:
				Rcpp::Rcout << "OGR: Corrupt data" << std::endl;     // #nocov
				break; // #nocov
			case OGRERR_FAILURE:
				Rcpp::Rcout << "OGR: index invalid?" << std::endl;    // #nocov
				break; // #nocov
			default:
				Rcpp::Rcout << "Error code: " << err << std::endl;    // #nocov
		}
		Rcpp::stop("OGR error");
	}
}

void set_config_options(Rcpp::CharacterVector ConfigOptions) {
	if (ConfigOptions.size()) {
		if (ConfigOptions.attr("names") == R_NilValue)
			Rcpp::stop("config_options should be a character vector with names, as in c(key=\"value\")");
		Rcpp::CharacterVector names = ConfigOptions.attr("names");
		for (int i = 0; i < ConfigOptions.size(); i++)
			CPLSetConfigOption(names[i], ConfigOptions[i]);
	}
}

void unset_config_options(Rcpp::CharacterVector ConfigOptions) {
	if (ConfigOptions.size()) {
		Rcpp::CharacterVector names = ConfigOptions.attr("names");
		for (int i = 0; i < ConfigOptions.size(); i++)
			CPLSetConfigOption(names[i], NULL);
	}
}

Rcpp::CharacterVector wkt_from_spatial_reference(const OGRSpatialReference *srs) { // FIXME: add options?
	char *cp;
#if GDAL_VERSION_MAJOR >= 3
	const char *options[3] = { "MULTILINE=YES", "FORMAT=WKT2", NULL };
	OGRErr err = srs->exportToWkt(&cp, options);
#else
	OGRErr err = srs->exportToPrettyWkt(&cp);
#endif
	if (err != OGRERR_NONE)
		Rcpp::stop("OGR error: cannot export to WKT"); // #nocov
	Rcpp::CharacterVector out(cp);
	CPLFree(cp);
	return out;
}

Rcpp::List fix_old_style(Rcpp::List crs) {
	if (crs.attr("names") == R_NilValue)
		Rcpp::stop("invalid crs object: no names");
	Rcpp::CharacterVector n = crs.attr("names");
	if (n.length() != 2)
		Rcpp::stop("invalid crs object: wrong length");
	if (n[0] == "epsg") { // create new: // #nocov start
		Rcpp::List ret(2);
		ret[0] = NA_STRING;
		ret[1] = NA_STRING;
		Rcpp::CharacterVector proj4string = crs(1);
		if (! Rcpp::CharacterVector::is_na(proj4string[0])) {
			ret[0] = proj4string[0]; // copy to $input
			OGRSpatialReference *srs = new OGRSpatialReference;
			srs = handle_axis_order(srs);
			handle_error(srs->SetFromUserInput((const char *) proj4string(0)));
			ret[1] = wkt_from_spatial_reference(srs); // copy to $wkt
			delete srs;
		}
		Rcpp::CharacterVector names(2);
		names(0) = "input";
		names(1) = "wkt";
		ret.attr("names") = names;
		ret.attr("class") = "crs";
		crs = ret; // #nocov end
	}
	return crs;
}

OGRSpatialReference *OGRSrs_from_crs(Rcpp::List crs) {
	// fix old-style crs:
	crs = fix_old_style(crs);
	OGRSpatialReference *dest = NULL;
	Rcpp::CharacterVector wkt = crs[1];
	if (! Rcpp::CharacterVector::is_na(wkt[0])) {
		dest = new OGRSpatialReference;
		dest = handle_axis_order(dest);
		char *cp = wkt[0];
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
// #if GDAL_VERSION_NUM <= 2020000 FIXME: breaks on gh actions
		handle_error(dest->importFromWkt(&cp));
#else
		handle_error(dest->importFromWkt((const char *) cp));
#endif
	}
	return dest;
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_parameters(Rcpp::List crs) {

	char *cp = NULL;
	unset_error_handler();

	OGRSpatialReference *srs = OGRSrs_from_crs(crs);
	if (srs == NULL)
		Rcpp::stop("crs not found"); // #nocov

	Rcpp::List out(16);
	Rcpp::CharacterVector names(16);
	out(0) = Rcpp::NumericVector::create(srs->GetSemiMajor());
	names(0) = "SemiMajor";

	out(1) = Rcpp::NumericVector::create(srs->GetSemiMinor());
	names(1) = "SemiMinor";

	Rcpp::NumericVector InvFlattening(1);
	OGRErr Err;
	srs->GetInvFlattening(&Err);
	if (Err == OGRERR_FAILURE)
		InvFlattening(0) = NA_REAL; // #nocov
	else
		InvFlattening(0) = srs->GetInvFlattening(NULL); // for +ellps=sphere, still zero :-(
	out(2) = InvFlattening;
	names(2) = "InvFlattening";

	out(3) = Rcpp::LogicalVector::create((bool) srs->IsGeographic());
	names(3) = "IsGeographic";

#if GDAL_VERSION_NUM > 2030000
	const char *unit;
#else
	char *unit;
#endif
	if (srs->IsGeographic())
		srs->GetAngularUnits(&unit);
	else
		srs->GetLinearUnits(&unit);
	if (unit == NULL || strncmp(unit, "unknown", 8) == 0)
		out(4) = Rcpp::CharacterVector::create(NA_STRING);
	else
		out(4) = Rcpp::CharacterVector::create(unit);
	names(4) = "units_gdal";

	out(5) = Rcpp::LogicalVector::create(srs->IsVertical());
	names(5) = "IsVertical";

	// wkt pretty:
	if (srs->exportToPrettyWkt(&cp) == OGRERR_NONE) {
		out(6) = Rcpp::CharacterVector::create(cp);
		CPLFree(cp);
	} else
		out(6) = "";
	names(6) = "WktPretty";

	// wkt:
	if (srs->exportToWkt(&cp) == OGRERR_NONE) {
		out(7) = Rcpp::CharacterVector::create(cp);
		CPLFree(cp);
	} else
		out(7) = "";
	names(7) = "Wkt";

#if GDAL_VERSION_MAJOR >= 3
	out(8) = Rcpp::CharacterVector::create(srs->GetName());
#else
	out(8) = Rcpp::CharacterVector::create("unknown");
#endif
	names(8) = "Name";

	// proj4string
	if (srs->exportToProj4(&cp) == OGRERR_NONE) {
		out(9) = Rcpp::CharacterVector::create(cp);
		CPLFree(cp);
	} else
		out(9) = Rcpp::CharacterVector::create(NA_STRING); // #nocov
	names(9) = "proj4string";

	// epsg
	if (srs->GetAuthorityCode(NULL) != NULL && strcmp(srs->GetAuthorityName(NULL), "EPSG") == 0)
		out(10) = Rcpp::IntegerVector::create(atoi(srs->GetAuthorityCode(NULL)));
	else
		out(10) = Rcpp::IntegerVector::create(NA_INTEGER);
	names(10) = "epsg";

	bool yx = srs->EPSGTreatsAsLatLong() || srs->EPSGTreatsAsNorthingEasting();
	out(11) = Rcpp::LogicalVector(yx);
	names(11) = "yx";

	// ProjJson:
#if GDAL_VERSION_NUM > 3010000
	if (srs->exportToPROJJSON(&cp, NULL) == OGRERR_NONE) {
		out(12) = Rcpp::CharacterVector::create(cp);
		CPLFree(cp);
	} else
		out(12) = "";
#endif
	names(12) = "ProjJson";

	// WKT1_ESRI
#if GDAL_VERSION_MAJOR >= 3
	const char *options[3] = { "MULTILINE=YES", "FORMAT=WKT1_ESRI", NULL };
	if (srs->exportToWkt(&cp, options) != OGRERR_NONE)
		out(13) = Rcpp::CharacterVector::create(NA_STRING); // FIXME: CPLFree() in this case?
	else {
		out(13) = Rcpp::CharacterVector::create(cp);
		CPLFree(cp);
	}
#else
	out(13) = "";
#endif
	names(13) = "WKT1_ESRI";

	// srid
	if (srs->GetAuthorityName(NULL) != NULL && srs->GetAuthorityCode(NULL) != NULL) {
		char str[101];
		snprintf(str, (size_t) 100, "%s:%s",
			srs->GetAuthorityName(NULL), srs->GetAuthorityCode(NULL));
		Rcpp::CharacterVector v = str;
		out(14) = v;
	} else
		out(14) = Rcpp::CharacterVector::create(NA_STRING);
	names(14) = "srid";

	// axes, 15:
#if GDAL_VERSION_NUM > 3000000
	int ac = srs->GetAxesCount();
#else
	int ac = 0;
#endif
	Rcpp::CharacterVector nms(ac);
	Rcpp::IntegerVector orientation(ac);
	Rcpp::NumericVector convfactor(ac);
#if GDAL_VERSION_NUM > 3040000
	for (int i = 0; i < ac; i++) {
		double pdfConvFactor;
		OGRAxisOrientation peOrientation;
		const char *ret = srs->GetAxis(srs->IsGeographic() ? "GEOGCS" : "PROJCS", 
						i, &peOrientation, &pdfConvFactor);
		if (ret != NULL) {
			nms[i] = ret;
			orientation[i] = (int) peOrientation;
			convfactor[i] = pdfConvFactor;
		} else {
			nms[i] = NA_STRING;
			orientation[i] = NA_INTEGER;
			convfactor[i] = NA_REAL;
		}
	}
#endif
	Rcpp::DataFrame axes_df = Rcpp::DataFrame::create(
		Rcpp::_["name"] = nms,
		Rcpp::_["orientation"] = orientation,
		Rcpp::_["convfactor"] = convfactor);
	out(15) = axes_df;
	names(15) = "axes";

	set_error_handler();

	delete srs;
	out.attr("names") = names;
	out.attr("class") = "crs_parameters";
	return out;
}

int srid_from_crs(Rcpp::List crs) {
	const char *cp;
	int ret_val = NA_INTEGER;
	unset_error_handler();
	OGRSpatialReference *ref = OGRSrs_from_crs(crs);
	if (ref && ref->AutoIdentifyEPSG() == OGRERR_NONE &&
			(cp = ref->GetAuthorityCode(NULL)) != NULL) {
		ret_val = atoi(cp);
	}
	if (ref != NULL)
		ref->Release();
	set_error_handler();
	return(ret_val);
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_crs_equivalent(Rcpp::List crs1, Rcpp::List crs2) {

	OGRSpatialReference *srs1 = OGRSrs_from_crs(crs1);
	OGRSpatialReference *srs2 = OGRSrs_from_crs(crs2);
	if (srs1 == NULL && srs2 == NULL) // #nocov start
		return Rcpp::LogicalVector::create(true);
	if (srs1 == NULL) {
		delete srs2;
		return Rcpp::LogicalVector::create(false);
	}
	if (srs2 == NULL) {
		delete srs1;
		return Rcpp::LogicalVector::create(false);
	} // #nocov end
#if GDAL_VERSION_MAJOR >= 3
	const char *options[3] = { NULL, NULL, NULL };
	if (axis_order_authority_compliant) {
		options[0] = "IGNORE_DATA_AXIS_TO_SRS_AXIS_MAPPING=NO";
		options[1] = "CRITERION=STRICT";
	} else
		options[0] = "IGNORE_DATA_AXIS_TO_SRS_AXIS_MAPPING=YES";
	bool b = (bool) srs1->IsSame(srs2, options);
#else
	bool b = (bool) srs1->IsSame(srs2);
#endif
	delete srs1;
	delete srs2;
	return Rcpp::LogicalVector::create(b);
}

std::vector<OGRGeometry *> ogr_from_sfc(Rcpp::List sfc, OGRSpatialReference **sref) {

	Rcpp::List wkblst = CPL_write_wkb(sfc, false);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRSpatialReference *local_srs = OGRSrs_from_crs(sfc.attr("crs"));

	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		OGRErr err = OGRGeometryFactory::createFromWkb(&(r[0]), local_srs, &(g[i]),
			r.length(), wkbVariantIso);
		if (err != OGRERR_NONE) {
			if (g[i] != NULL) // release: #nocov
				OGRGeometryFactory::destroyGeometry(g[i]); // #nocov
			if (local_srs != NULL)      // #nocov start
				local_srs->Release();
			handle_error(err);          // #nocov end
		}
	}
	if (sref != NULL)
		*sref = local_srs; // return and release later, or
	else if (local_srs != NULL)
		local_srs->Release(); // release now
	return g;
}

std::vector<char *> create_options(Rcpp::CharacterVector lco, bool quiet) {
	if (lco.size() == 0)
		quiet = true; // nothing to report
	if (! quiet)
		Rcpp::Rcout <<  "options:        "; // #nocov
	std::vector<char *> ret(lco.size() + 1);
	for (int i = 0; i < lco.size(); i++) {
		ret[i] = (char *) (lco[i]);
		if (! quiet)
			Rcpp::Rcout << ret[i] << " "; // #nocov
	}
	ret[lco.size()] = NULL;
	if (! quiet)
		Rcpp::Rcout << std::endl;         // #nocov
	return ret;
}

// convert NULL-terminated array of strings to Rcpp::CharacterVector
Rcpp::CharacterVector charpp2CV(char **cp) {
	int n = 0;
	while (cp && cp[n] != NULL)
		n++; // count
	Rcpp::CharacterVector ret(n);
	for (int i = 0; i < n; i++)
		ret(i) = cp[i];
	return ret;
}

Rcpp::List create_crs(const OGRSpatialReference *ref, bool set_input) {
	Rcpp::List crs(2);
	if (ref == NULL) {
		crs(0) = Rcpp::CharacterVector::create(NA_STRING);
		crs(1) = Rcpp::CharacterVector::create(NA_STRING);
	} else {
		if (set_input) {
#if GDAL_VERSION_MAJOR >= 3
			crs(0) = Rcpp::CharacterVector::create(ref->GetName());
#else
			const char *cp;
			OGRSpatialReference ref_cp = *ref;
			if (ref_cp.AutoIdentifyEPSG() == OGRERR_NONE && // ->AutoIdentifyEPSG() breaks if "this" is const
					(cp = ref_cp.GetAuthorityCode(NULL)) != NULL)
				crs(0) = cp;
			else
				crs(0) = Rcpp::CharacterVector::create(NA_STRING);
#endif
		}
		crs(1) = wkt_from_spatial_reference(ref);
	}
	Rcpp::CharacterVector nms(2);
	nms(0) = "input";
	nms(1) = "wkt";
	crs.attr("names") = nms;
	crs.attr("class") = "crs";
	return crs;
}

Rcpp::List sfc_from_ogr(std::vector<OGRGeometry *> g, bool destroy = false) {
	OGRwkbGeometryType type = wkbGeometryCollection;
	Rcpp::List lst(g.size());
	Rcpp::List crs = create_crs(g.size() && g[0] != NULL ? g[0]->getSpatialReference() : NULL);
	for (size_t i = 0; i < g.size(); i++) {
		if (g[i] == NULL)
			g[i] = OGRGeometryFactory::createGeometry(type); // #nocov
		else
			type = g[i]->getGeometryType();
		Rcpp::RawVector raw(g[i]->WkbSize());
		handle_error(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		if (destroy)
			OGRGeometryFactory::destroyGeometry(g[i]);
	}
	Rcpp::List ret = CPL_read_wkb(lst, false, false);
	ret.attr("crs") = crs;
	ret.attr("class") = "sfc";
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_input(Rcpp::CharacterVector input) {
	OGRSpatialReference *ref = new OGRSpatialReference;
	handle_axis_order(ref);
	Rcpp::List crs;
	if (ref->SetFromUserInput(input[0]) == OGRERR_NONE) {
		crs = create_crs(ref, false);
		crs(0) = input;
	} else
		crs = create_crs(NULL);
	delete ref;
	return crs;
}

// [[Rcpp::export]]
Rcpp::List CPL_roundtrip(Rcpp::List sfc) { // for debug purposes
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		char *out;
		g[i]->exportToWkt(&out);
		Rcpp::Rcout << out << std::endl;
		CPLFree(out);
	}
	return sfc_from_ogr(g, true); // destroys g;
}

// [[Rcpp::export]]
Rcpp::List CPL_circularstring_to_linestring(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCircularString *cs = (OGRCircularString *) g[i];
		out[i] = cs->CurveToLine();
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_multisurface_to_multipolygon(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRMultiSurface *cs = (OGRMultiSurface *) g[i];
		if (cs->hasCurveGeometry(true)) {
			out[i] = cs->getLinearGeometry();
			OGRGeometryFactory::destroyGeometry(g[i]);
		} else
			out[i] = cs->CastToMultiPolygon(cs); // consumes cs #nocov
		if (out[i] == NULL)
			Rcpp::stop("CPL_multisurface_to_multipolygon: NULL returned - non-polygonal surface?"); // #nocov
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_compoundcurve_to_linear(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCompoundCurve *cs = (OGRCompoundCurve *) g[i];
		out[i] = cs->getLinearGeometry();
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_curve_to_linestring(Rcpp::List sfc) { // need to pass more parameters? #nocov start
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCurve *cs = (OGRCurve *) g[i];
		out[i] = cs->CastToLineString(cs);
	}
	return sfc_from_ogr(out, true); // destroys out;
} // #nocov end

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_can_transform(Rcpp::List src, Rcpp::List dst) {
	if (src.size() != 2 || dst.size() != 2)
		return false;
	Rcpp::CharacterVector src_cv = src[0];
	Rcpp::CharacterVector dst_cv = dst[0];
	if (Rcpp::CharacterVector::is_na(src_cv[0]) || Rcpp::CharacterVector::is_na(dst_cv[0]))
		return false;
	OGRSpatialReference *srs_src = OGRSrs_from_crs(src);
	OGRSpatialReference *srs_dst = OGRSrs_from_crs(dst);
	unset_error_handler();
	OGRCoordinateTransformation *ct = OGRCreateCoordinateTransformation(srs_src, srs_dst);
	set_error_handler();
	delete srs_src;
	delete srs_dst;
	if (ct) {
		ct->DestroyCT(ct);
		return true;
	} else
		return false;
}

// [[Rcpp::export]]
Rcpp::List CPL_transform(Rcpp::List sfc, Rcpp::List crs,
		Rcpp::NumericVector AOI, Rcpp::CharacterVector pipeline, bool reverse = false,
		double desired_accuracy = -1.0, bool allow_ballpark = true) {

	// transform geometries:
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	if (g.size() == 0)
		return sfc_from_ogr(g, true); // destroys g

	OGRSpatialReference *dest = NULL;
	// if pipeline was not set, import crs to dest:
	if (pipeline.size() == 0 && !(dest = OGRSrs_from_crs(crs)))
		Rcpp::stop("crs not found: is it missing?"); // #nocov

#if GDAL_VERSION_MAJOR >= 3
	OGRCoordinateTransformationOptions *options = new OGRCoordinateTransformationOptions;
	if (pipeline.size() && !options->SetCoordinateOperation(pipeline[0], reverse))
		Rcpp::stop("pipeline value not accepted");
	if (AOI.size() == 4 && !options->SetAreaOfInterest(AOI[0], AOI[1], AOI[2], AOI[3]))
		Rcpp::stop("values for area of interest not accepted");
#if GDAL_VERSION_MINOR >= 3
	options->SetDesiredAccuracy(desired_accuracy);
	options->SetBallparkAllowed(allow_ballpark);
#endif
	// unset_error_handler(); // FIXME: is this always a good idea?
	OGRCoordinateTransformation *ct =
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest, *options);
	// set_error_handler();
	delete options;
#else
	if (pipeline.size() || AOI.size())
		Rcpp::stop("pipeline or area of interest require GDAL >= 3"); // #nocov
	OGRCoordinateTransformation *ct =
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
#endif
	if (ct == NULL) {
		if (dest)
			dest->Release(); // #nocov start
		sfc_from_ogr(g, true); // to destroy g
		Rcpp::stop("OGRCreateCoordinateTransformation(): transformation not available"); // #nocov end
	}
	for (size_t i = 0; i < g.size(); i++) {
		CPLPushErrorHandler(CPLQuietErrorHandler);
		OGRErr err = 0;
		if (! g[i]->IsEmpty())
			err = g[i]->transform(ct);
		CPLPopErrorHandler();
		if (err == 1 || err == 6) {
			OGRwkbGeometryType geomType = g[i]->getGeometryType();
			OGRGeometryFactory::destroyGeometry(g[i]);
			g[i] = OGRGeometryFactory::createGeometry(geomType); // return empty geometry of this type
		} else
			handle_error(err);
	}

	Rcpp::List ret = sfc_from_ogr(g, true); // destroys g;
	// how to return the target CRS when only a transformation pipeline is provided? Not by:
	// ret.attr("crs") = create_crs(ct->GetTargetCS(), true);
	ct->DestroyCT(ct);
	if (dest)
		dest->Release();
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_wrap_dateline(Rcpp::List sfc, Rcpp::CharacterVector opt, bool quiet = true) {

	std::vector <char *> options = create_options(opt, quiet);
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> ret(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		ret[i] = OGRGeometryFactory::transformWithOptions(g[i], NULL, options.data());
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(ret, true); // destroys ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_get_gdal_drivers(int dummy) {

	int ndr = GetGDALDriverManager()->GetDriverCount();
	Rcpp::CharacterVector name(ndr);
	Rcpp::CharacterVector long_name(ndr);
	Rcpp::LogicalVector create(ndr);
	Rcpp::LogicalVector copy(ndr);
	Rcpp::LogicalVector vattr(ndr);
	Rcpp::LogicalVector rattr(ndr);
	Rcpp::LogicalVector vsi_attr(ndr);
	for (int i = 0; i < ndr; i++) {
		GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
		name(i) = GDALGetDriverShortName( pDriver );
		long_name(i) = GDALGetDriverLongName( pDriver );
		create(i) = (pDriver->GetMetadataItem(GDAL_DCAP_CREATE) != NULL);
		copy(i) =   (pDriver->GetMetadataItem(GDAL_DCAP_CREATECOPY) != NULL);
		vattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_VECTOR) != NULL);
		rattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_RASTER) != NULL);
		vsi_attr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_VIRTUALIO) != NULL);
	}
	return Rcpp::DataFrame::create(
		Rcpp::Named("name") = name,
		Rcpp::Named("long_name") = long_name,
		Rcpp::Named("write") = create,
		Rcpp::Named("copy") = copy,
		Rcpp::Named("is_raster") = rattr,
		Rcpp::Named("is_vector") = vattr,
		Rcpp::Named("vsi") = vsi_attr);
}

// [[Rcpp::export]]
Rcpp::List CPL_sfc_from_wkt(Rcpp::CharacterVector wkt) {
	std::vector<OGRGeometry *> g(wkt.size());
	OGRGeometryFactory f;
	for (int i = 0; i < wkt.size(); i++) {
		char *wkt_str = wkt(i);
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
		handle_error(f.createFromWkt(&wkt_str, NULL, &(g[i])));
#else
		handle_error(f.createFromWkt( (const char*) wkt_str, NULL, &(g[i])));
#endif
	}
	return sfc_from_ogr(g, true);
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdal_with_geos() {
	bool withGEOS = OGRGeometryFactory::haveGEOS();
	return Rcpp::LogicalVector::create(withGEOS);
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_axis_order_authority_compliant(Rcpp::LogicalVector authority_compliant) {
	if (authority_compliant.size() > 1)
		Rcpp::stop("argument authority_compliant should have length 0 or 1"); // #nocov
#if GDAL_VERSION_NUM < 2050000
	if (authority_compliant.size() == 1 && authority_compliant[0])
		Rcpp::stop("For setting axis order compliancy, GDAL >= 2.5.0 is required"); // #nocov
#endif
	bool old_value = axis_order_authority_compliant;
	if (authority_compliant.size() == 1)
		axis_order_authority_compliant = authority_compliant[0];
	return Rcpp::LogicalVector::create(old_value);
}

OGRSpatialReference *handle_axis_order(OGRSpatialReference *sr) {
#if GDAL_VERSION_NUM >= 2050000
	if (sr != NULL) {
		if (!axis_order_authority_compliant)
			sr->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
		else
			sr->SetAxisMappingStrategy(OAMS_AUTHORITY_COMPLIANT);
	}
#endif
	return sr;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_get_proj_search_paths(Rcpp::CharacterVector paths) {
#if GDAL_VERSION_NUM >= 3000300
	char **cp = OSRGetPROJSearchPaths();
	Rcpp::CharacterVector ret = charpp2CV(cp);
	CSLDestroy(cp);
	return ret;
#else
	Rcpp::stop("GDAL >= 3.0.3 required");
#endif
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_set_proj_search_paths(Rcpp::CharacterVector paths) {
#if GDAL_VERSION_NUM >= 3000000
	std::vector <char *> paths_char;
	if (paths.size()) {
		paths_char = create_options(paths, true);
		OSRSetPROJSearchPaths(paths_char.data());
	}
#else
	Rcpp::stop("GDAL >= 3.0.0 required");
#endif
	return paths;
}
