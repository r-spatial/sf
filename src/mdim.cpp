// https://github.com/rouault/gdal/blob/rfc75/gdal/doc/source/tutorials/multidimensional_api_tut.rst
#include "gdal_priv.h"

#include <Rcpp.h>

#define NO_GDAL_CPP_HEADERS
#include "gdal.h"
#include "gdal_sf_pkg.h"

using namespace Rcpp;

#if defined(__MINGW32__) && !defined(__MINGW64__)
#define WIN32BIT
#endif

#if GDAL_VERSION_NUM >= 3040000 && !(defined(WIN32BIT))
CharacterVector get_attributes(std::vector<std::shared_ptr<GDALAttribute>> a) {
	CharacterVector l(a.size());
	CharacterVector na(a.size());
	for (int i = 0; i < a.size(); i++) {
		l[i] = a[i]->ReadAsString();
		na[i] = a[i]->GetName();
	}
	if (a.size())
		l.attr("names") = na;
	return l;
}
#endif

#if GDAL_VERSION_NUM >= 3010000 && !(defined(WIN32BIT))
List get_dimension_values(std::shared_ptr<GDALMDArray> array) {
	size_t nValues = 1;
	std::vector<size_t> anCount;
	IntegerVector dims;
	std::vector<GUInt64> offset;
	for (const auto poDim: array->GetDimensions()) {
		anCount.push_back(static_cast<size_t>(poDim->GetSize()));
		dims.push_back(static_cast<size_t>(poDim->GetSize()));
		offset.push_back(0);
		nValues *= anCount.back();
	}
#if GDAL_VERSION_NUM >= 3040000 && !(defined(WIN32BIT))
	CharacterVector att = get_attributes(array->GetAttributes());
#else
	CharacterVector att;
#endif
	List ret(1);
	if (array->GetDataType().GetClass() == GEDTC_NUMERIC) {
		NumericVector vec( nValues );
		bool ok = array->Read(offset.data(),
					anCount.data(),
					nullptr, /* step: defaults to 1,1,1 */
					nullptr, /* stride: default to row-major convention */
					GDALExtendedDataType::Create(GDT_Float64),
					vec.begin());
		if (!ok)
			Rcout << "cannot convert values for array " << array->GetName() << std::endl;
		vec.attr("dim") = dims;
		vec.attr("units") = array->GetUnit();
		if (att.size())
			vec.attr("attributes") = att;
		ret[0] = vec;
	} else {
		// CharacterVector vec(nValues);
		std::vector<char *> vec(nValues);
		bool ok = array->Read(offset.data(),
					anCount.data(),
					nullptr, /* step: defaults to 1,1,1 */
					nullptr, /* stride: default to row-major convention */
					GDALExtendedDataType::CreateString(100),
					vec.data());
		if (!ok)
			Rcout << "cannot convert values for array " << array->GetName() << std::endl;
		CharacterVector cv(nValues);
		for (size_t i = 0; i < nValues; i++)
			cv[i] = vec[i];
		if (att.size())
			cv.attr("attributes") = att;
		ret[0] = cv;
	}
	return ret;
}

List get_dimension(const std::shared_ptr<GDALDimension> dim) {
	List dimension = List::create(
		_["from"] = IntegerVector::create(1),
		_["to"] = IntegerVector::create(dim->GetSize()),
		_["values"] = get_dimension_values(dim->GetIndexingVariable()),
		_["type"] = CharacterVector::create(dim->GetType()),
		_["direction"] = CharacterVector::create(dim->GetDirection())
		);
	return dimension;
}

// [[Rcpp::export]]
List read_mdim(CharacterVector file, CharacterVector array_names, CharacterVector oo,
				IntegerVector offset, IntegerVector count, IntegerVector step) {

	std::vector <char *> oo_char = create_options(oo, true); // open options
	auto poDataset = std::unique_ptr<GDALDataset>(
		GDALDataset::Open((const char *) file[0], GDAL_OF_MULTIDIM_RASTER | GDAL_OF_VERBOSE_ERROR,
			nullptr, oo_char.data(), nullptr));
	if( !poDataset )
		stop("file not found");

	auto poRootGroup = poDataset->GetRootGroup();
	if( !poRootGroup )
		stop("cannot open root group");

	auto curGroup = poRootGroup;
	auto groupNames = poRootGroup->GetGroupNames();
	if (groupNames.size() > 0) {
		curGroup = curGroup->OpenGroup(groupNames[0]);
		if (!curGroup) {
			Rcout << "group: " << groupNames[0] << ";" << std::endl;
			stop("Cannot find group");
		}
	}

	// Rcout << "name: " << curGroup->GetName() << " full_name: " << curGroup->GetFullName() << std::endl;
	if (array_names.size() == 0) { // sort out: find the one(s) with the most dimensions
		int ndim = 0;
		int largest_size = 0;
		for (const auto an: curGroup->GetMDArrayNames()) { // all:
			auto a(curGroup->OpenMDArray(an));
			ndim = a->GetDimensions().size();
			if (ndim > largest_size)
				largest_size = ndim;
		}
		for (const auto an: curGroup->GetMDArrayNames()) { // all:
			auto a(curGroup->OpenMDArray(an));
			ndim = a->GetDimensions().size();
			if (ndim == largest_size)
				array_names.push_back(an);
		}
	}
	int n = array_names.size();

	// how many arrays have identical dimensions to the first?
	const char *name = array_names[0];
	auto array(curGroup->OpenMDArray(name));
	if (!array)
		stop("Cannot find array");
	if (offset.size() != 0 && offset.size() != array->GetDimensionCount())
		stop("offset has wrong size");
	if (count.size() != 0 && count.size() != array->GetDimensionCount())
		stop("count has wrong size");
	if (step.size() != 0 && step.size() != array->GetDimensionCount())
		stop("step has wrong size");

	size_t nValues = 1;
	std::vector<size_t> anCount;
	std::vector<GInt64> stp;
	IntegerVector dims;
	CharacterVector dim_names;
	std::vector<GUInt64> offst;
	List dimensions;
	int i = 0;
	for (const auto poDim: array->GetDimensions()) {
		dim_names.push_back(poDim->GetName());
		if (offset.size() == 0)
			offst.push_back(0);
		else
			offst.push_back(offset[i]);
		if (step.size() == 0)
			stp.push_back(1);
		else
			stp.push_back(step[i]);
		if (count.size() == 0)
			anCount.push_back((poDim->GetSize() - offst.back())/stp.back());
		else
			anCount.push_back(count[i]);
		dims.push_back(anCount.back());
		nValues *= anCount.back();
		/*
		if (count.size() > i)
			Rcout << "count[i]: " << count[i] << "\n";
		Rcout << "nValues: " << nValues << "\n";
		Rcout << "stp[i]: " << stp[i] << "\n";
		Rcout << "anCount[i]: " << anCount[i] << "\n";
		Rcout << "offst[i]: " << offst[i] << "\n";
		*/
		List dimension(get_dimension(poDim));
		dimensions.push_back(dimension);
		i++;
	}
	List vec_lst(n);
	CharacterVector a_names(n);
	for (int i = 0; i < n; i++) {
		name = array_names[i];
		a_names[i] = array_names[i];
		auto arr(curGroup->OpenMDArray(name));
		NumericVector vec( nValues );
		bool ok = arr->Read(offst.data(),
					anCount.data(),
					(const GInt64*) stp.data(), /* step: defaults to 1,1,1 */
					nullptr, /* stride: default to row-major convention */
					GDALExtendedDataType::Create(GDT_Float64),
					vec.begin());
		if (!ok)
			Rcout << "Read failed for array " << name << std::endl;
		dims.attr("names") = dim_names;
		dimensions.attr("names") = dim_names;
		bool has_offset = false;
		double offst = arr->GetOffset(&has_offset);
		if (!has_offset)
			offst = 0.0;
		bool has_scale = false;
		double scale = arr->GetScale(&has_scale);
		if (!has_scale)
			scale = 1.0;
		bool has_nodata = false;
		double nodata_value = arr->GetNoDataValueAsDouble(&has_nodata);
		if (has_offset || has_scale || has_nodata) {
			for (size_t i = 0; i < nValues; i++) {
				if (ISNAN(vec[i]) || (has_nodata && vec[i] == nodata_value))
					vec[i] = NA_REAL;
				else
					vec[i] = vec[i] * scale + offst;
			}
		}
		vec.attr("dim") = dims;
		vec.attr("units") = arr->GetUnit();
		vec_lst[i] = vec;
	}
	vec_lst.attr("names") = a_names;
	std::shared_ptr<OGRSpatialReference> srs = array->GetSpatialRef();
	List ret = List::create(
		_["array_list"] = vec_lst,
		_["dimensions"] = dimensions,
		_["srs"] = srs == nullptr ? CharacterVector::create(NA_STRING) : wkt_from_spatial_reference(srs.get())
	);
	return ret;
}

// [[Rcpp::export]]
List write_mdim(List x, CharacterVector file, List dimensions, CharacterVector units) {

	stop("not implemented yet");

	// GDALDatasetH hDstDS = nullptr;
	/* if( psOptionsForBinary->bUpdate )
	{
		CPLPushErrorHandler(CPLQuietErrorHandler);
		hDstDS = GDALOpenEx(
			psOptionsForBinary->pszDest,
			GDAL_OF_RASTER | GDAL_OF_MULTIDIM_RASTER | GDAL_OF_VERBOSE_ERROR | GDAL_OF_UPDATE,
			nullptr, nullptr, nullptr );
		CPLPopErrorHandler();
	}
	*/

	const char *pszDest = file[0];
	CPLString osFormat("");
	if( EQUAL(CPLGetExtension(pszDest), "nc") )
		osFormat = "netCDF";
	/*
	else
		osFormat = GetOutputDriverForRaster(pszDest);
	*/
	if( osFormat.empty() )
		stop("format not found");
	GDALDriver *poDriver = GDALDriver::FromHandle(GDALGetDriverByName( osFormat ));
	char** papszDriverMD = poDriver ? poDriver->GetMetadata(): nullptr;
	if( poDriver == nullptr
		|| (!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_RASTER, "FALSE")) &&
			!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_MULTIDIM_RASTER, "FALSE")))
		|| (!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_CREATE, "FALSE")) &&
			!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_CREATECOPY, "FALSE")) &&
			!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_CREATE_MULTIDIMENSIONAL, "FALSE")) &&
			!CPLTestBool(CSLFetchNameValueDef(papszDriverMD, GDAL_DCAP_CREATECOPY_MULTIDIMENSIONAL, "FALSE"))) )
				stop("output driver does not support creation");


	/*
	GDALDatasetH hRetDS = GDALMultiDimTranslate(
										psOptionsForBinary->pszDest,
					                    hDstDS,
                                        1, &hInDS,
                                        psOptions, &bUsageError);
										*/

	return x;
}

#else
List read_mdim(CharacterVector file, CharacterVector array_names, CharacterVector oo) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
List write_mdim(List x, CharacterVector file, List dimensions, CharacterVector units) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
#endif
