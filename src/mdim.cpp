// https://github.com/rouault/gdal/blob/rfc75/gdal/doc/source/tutorials/multidimensional_api_tut.rst
#include "gdal_priv.h"

#include <Rcpp.h>

#define NO_GDAL_CPP_HEADERS
#include "gdal.h"
#include "gdal_sf_pkg.h"

using namespace Rcpp;

NumericVector get_dimension_values(std::shared_ptr<GDALMDArray> array) {
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
	NumericVector vec( nValues );
	bool ok = array->Read(offset.data(),
				anCount.data(),
				nullptr, /* step: defaults to 1,1,1 */
				nullptr, /* stride: default to row-major convention */
				GDALExtendedDataType::Create(GDT_Float64),
				vec.begin());
	if (!ok) {
			Rcout << "cannot convert values for array: " << array->GetName() << std::endl;
	}
	vec.attr("dim") = dims;
	vec.attr("units") = array->GetUnit();
	return vec;
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
List read_mdim(CharacterVector file, CharacterVector array_name, CharacterVector oo) {

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
		if( !curGroup ) {
			Rcout << "group: " << groupNames[0] << ";" << std::endl;
			stop("Cannot find group");
		}
	}

	const char* pszArrayName = array_name[0];
	auto array(curGroup->OpenMDArray(pszArrayName));
	if( !array )
		stop("Cannot find array");
	/*
	DumpArray(poDS, array, serializer, psOptions,
			  alreadyDumpedDimensions, true, true);
	*/

	size_t nValues = 1;
	std::vector<size_t> anCount;
	IntegerVector dims;
	CharacterVector dim_names;
	std::vector<GUInt64> offset;
	List dimensions;
	for (const auto poDim: array->GetDimensions()) {
		anCount.push_back(static_cast<size_t>(poDim->GetSize()));
		dims.push_back(static_cast<size_t>(poDim->GetSize()));
		dim_names.push_back(poDim->GetName());
		offset.push_back(0);
		nValues *= anCount.back();
		List dimension(get_dimension(poDim));
		dimensions.push_back(dimension);
	}
	NumericVector vec( nValues );
	array->Read(offset.data(),
				anCount.data(),
				nullptr, /* step: defaults to 1,1,1 */
				nullptr, /* stride: default to row-major convention */
				GDALExtendedDataType::Create(GDT_Float64),
				vec.begin());
	dims.attr("names") = dim_names;
	dimensions.attr("names") = dim_names;
	vec.attr("dim") = dims;
	List other = List::create();
	bool has = false;
	double value = array->GetOffset(&has);
	if (has)
		other.attr("offset") = value;
	value = array->GetScale(&has);
	if (has)
		other.attr("offset") = value;
	value = array->GetNoDataValueAsDouble(&has);
	if (has)
		other.attr("NoDataValue") = value;
	std::shared_ptr<OGRSpatialReference> srs = array->GetSpatialRef();
	List ret = List::create(
		_["array"] = vec,
		_["dimensions"] = dimensions,
		_["units"] = array->GetUnit(),
		_["other"] = other,
		_["srs"] = wkt_from_spatial_reference(srs.get())
	);
	return ret;
}
