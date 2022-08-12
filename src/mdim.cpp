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
	for (size_t i = 0; i < a.size(); i++) {
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
	List ret(1);
	if (array == nullptr) {
		warning("array is NULL");
		return ret; // FIXME but this is an error essentially.
	}
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
	if (dim == nullptr)
		stop("dim is NULL");
	List dv;
	if (dim->GetIndexingVariable() == nullptr) {
		NumericVector nv(dim->GetSize());
		for (size_t i = 0; i < dim->GetSize(); i++)
			nv[i] = i + 1.0;
		dv = List::create(nv);
	} else
		dv = get_dimension_values(dim->GetIndexingVariable());
	List dimension = List::create(
		_["from"] = IntegerVector::create(1),
		_["to"] = IntegerVector::create(dim->GetSize()),
		_["values"] = dv,
		_["type"] = CharacterVector::create(dim->GetType()),
		_["direction"] = CharacterVector::create(dim->GetDirection())
		);
	return dimension;
}

// [[Rcpp::export]]
List CPL_read_mdim(CharacterVector file, CharacterVector array_names, CharacterVector oo,
				IntegerVector offset, IntegerVector count, IntegerVector step, 
				bool proxy = false, bool debug = false) {

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
	if (array_names.size() == 0) { // find the one(s) with the most dimensions:
		int ndim = 0;
		int largest_size = 0;
		for (const auto an: curGroup->GetMDArrayNames()) { // find largest size:
			auto a(curGroup->OpenMDArray(an));
			ndim = a->GetDimensions().size();
			if (ndim > largest_size)
				largest_size = ndim;
		}
		for (const auto an: curGroup->GetMDArrayNames()) { // identify target arrays:
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
	if (offset.size() != 0 && (size_t) offset.size() != array->GetDimensionCount())
		stop("offset has wrong size");
	if (count.size() != 0 && (size_t) count.size() != array->GetDimensionCount())
		stop("count has wrong size");
	if (step.size() != 0 && (size_t) step.size() != array->GetDimensionCount())
		stop("step has wrong size");
	if (proxy && (offset.size() != 0 || count.size() != 0 || step.size() != 0))
		stop("if proxy=TRUE, do not set offset, count or step, use these when reading data (downsample)");

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
		if (debug) {
			Rcout << "Dimension name: " << poDim->GetName() << "\n";
			if (count.size() > i)
				Rcout << "count[i]: " << count[i] << "\n";
			Rcout << "nValues: " << nValues << "\n";
			Rcout << "stp[i]: " << stp[i] << "\n";
			Rcout << "anCount[i]: " << anCount[i] << "\n";
			Rcout << "offst[i]: " << offst[i] << "\n";
			Rcout << "dims[i]: " << dims[i] << "\n";
		}
		List dimension(get_dimension(poDim));
		dimensions.push_back(dimension); // mind the "s"
		i++;
	}
	List vec_lst(n);
	CharacterVector a_names(n);
	for (int i = 0; i < n; i++) {
		name = array_names[i];
		a_names[i] = array_names[i];
		auto arr(curGroup->OpenMDArray(name));
		dims.attr("names") = dim_names;
		dimensions.attr("names") = dim_names;
		NumericVector vec;
		if (! proxy) { // read the arrays:
			NumericVector vec_(nValues);
			if (debug)
				Rcout << "size of vec_: " << vec_.size() << "\n";
			bool ok = arr->Read(offst.data(),
						anCount.data(),
						(const GInt64*) stp.data(), /* step: defaults to 1,1,1 */
						nullptr, /* stride: default to row-major convention */
						GDALExtendedDataType::Create(GDT_Float64),
						vec_.begin());
			if (!ok)
				Rcout << "Read failed for array " << name << std::endl;
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
					if (ISNAN(vec_[i]) || (has_nodata && vec_[i] == nodata_value))
						vec_[i] = NA_REAL;
					else
						vec_[i] = vec_[i] * scale + offst;
				}
			}
			vec_.attr("dim") = dims;
			vec_.attr("units") = arr->GetUnit();
			vec = vec_;
		}
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


void write_attributes(std::shared_ptr<GDALMDArray> md, CharacterVector attrs) {
	if (attrs.size() > 0) {
		CharacterVector names = attrs.attr("names");
		std::vector<GUInt64> empty;
		for (int i = 0; i < attrs.size(); i++) {
			const char *name = names[i];
			std::shared_ptr<GDALAttribute> at = 
					md->CreateAttribute(name, empty,  GDALExtendedDataType::CreateString(0), nullptr);
			if (at == nullptr) {
				Rcout << names[i] << ":" << std::endl;
				warning("could not create attribute: does it already exist? (skipping)");
			} else
				at->Write(attrs[i]);
		}
	}
}

// [[Rcpp::export]]
List CPL_write_mdim(CharacterVector name, CharacterVector driver, IntegerVector dimensions,
				List variables, CharacterVector wkt, CharacterVector xy, CharacterVector RootGroupOptions,
				CharacterVector CreationOptions) {
	
	if (name.size() != 1)
		stop("name should have length 1");
	if (driver.size() != 1)
		stop("driver should have length 1");
	GDALDriver *nc = GetGDALDriverManager()->GetDriverByName(driver[0]);
	if (nc == NULL)
		stop("cannot open driver");
	OGRSpatialReference *dest = NULL;
	if (wkt.size()) {
		char *cp = wkt[0];
		dest = new OGRSpatialReference;
		dest->importFromWkt((const char *) cp);
	}

	// create n-D array
	std::vector <char *> rgo = create_options(RootGroupOptions, true);
	std::vector <char *> co = create_options(CreationOptions, true); // open options
	GDALDataset *md = nc->CreateMultiDimensional(name[0], 
					RootGroupOptions.size() ? rgo.data() : nullptr, 
					CreationOptions.size()  ? co.data() : nullptr);
	if (md == NULL)
		stop("Cannot create MD array on this driver");
	std::shared_ptr<GDALGroup> g = md->GetRootGroup();
	if (g == NULL)
		stop("Cannot get RootGroup");

	// create dimensions on g:
	CharacterVector names; 
	if (dimensions.attr("names") != R_NilValue)
		names = dimensions.attr("names");
	else
		stop("dimensions should have names");
	std::vector<std::shared_ptr<GDALDimension>> all_dims;
	for (int i = dimensions.size() - 1; i >= 0; i--) { // backwards, for whatever reason
		std::string type;
		if (names[i] == xy[0]) // "x"
			type = "HORIZONTAL_X";
		else if (names[i] == xy[1]) // "y"
			type = "HORIZONTAL_Y";
		else if (names[i] == "time")
			type = "TEMPORAL";
		else
			type = "";
		const char *name = names[i];
		std::shared_ptr<GDALDimension> d = g->CreateDimension(name, type, "", dimensions[i], nullptr);
		if (d == nullptr)
			stop("creation of dimension failed");
		all_dims.push_back(d);
	}
	std::reverse(all_dims.begin(), all_dims.end()); // because I can't think backwards

	// create & write variables to g; write attributes
	GDALExtendedDataType dbl = GDALExtendedDataType::Create(GDT_Float64);
	if (variables.attr("names") != R_NilValue)
		names = variables.attr("names");
	else
		stop("variables should have names");
	LogicalVector which_crs;
	if (variables.attr("which_crs") != R_NilValue)
		which_crs = variables.attr("which_crs");
	else
		stop("which_crs attribute missing");

	for (int i = 0; i < variables.size(); i++) {
		NumericVector a = variables[i];
		IntegerVector which_dims;
		if (a.attr("which_dims") == R_NilValue)
			stop("variable has no attribute which_dims");
		else
			which_dims = a.attr("which_dims");
		std::vector<std::shared_ptr<GDALDimension>> dims;
		for (int i = which_dims.size() - 1; i >= 0; i--) {
			if (which_dims[i] == NA_INTEGER)
				stop("NA value in which_dims: logic error");
			dims.push_back(all_dims[which_dims[i]]);
		}
		const char *name = names[i];
		std::shared_ptr<GDALMDArray> mda = g->CreateMDArray(name, dims, dbl, nullptr);
		if (dest != NULL && which_crs[i] && !mda->SetSpatialRef(dest))
			warning("failed to assign CRS to array");
		if (a.size() != 0) {
			std::vector<GUInt64> start;
			std::vector<size_t> count;
			for (int i = dims.size() - 1; i >= 0; i--) {
				start.push_back(0); // FIXME: modify if updating sub-array
				count.push_back(dimensions[which_dims[i]]);
			}
			mda->Write(start.data(), count.data(), nullptr, nullptr, dbl, &(a[0]), nullptr, 0);
		}
		if (a.attr("attrs") != R_NilValue)
			write_attributes(mda, a.attr("attrs"));
	}

	// close, free & return:
	GDALClose(md);
	if (dest != NULL)
		delete dest;
	return variables;
}

#else
List CPL_read_mdim(CharacterVector file, CharacterVector array_names, CharacterVector oo,
				IntegerVector offset, IntegerVector count, IntegerVector step, 
				bool proxy = false, bool debug = false) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}

List CPL_write_mdim(CharacterVector name, CharacterVector driver, IntegerVector dimensions,
				List variables, CharacterVector wkt, CharacterVector xy, CharacterVector RootGroupOptions,
				CharacterVector CreationOptions) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
#endif
