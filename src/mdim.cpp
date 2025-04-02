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
	CharacterVector d_names;
	for (const auto &poDim: array->GetDimensions()) {
		anCount.push_back(static_cast<size_t>(poDim->GetSize()));
		dims.push_back(static_cast<size_t>(poDim->GetSize()));
		d_names.push_back(poDim->GetName());
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
		vec.attr("d_names") = d_names;
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

// if present, return geometry elements (coordinates, indexes), else return empty list
List get_geometry(std::shared_ptr<GDALGroup> curGroup) {
	List lst;
	for (const auto &an: curGroup->GetMDArrayNames()) {
		auto a(curGroup->OpenMDArray(an));
		if (a == nullptr) {
			Rcout << "could not open geometry array " << an << std::endl;
			stop("get_geometry(): cannot OpenMDArray()");
		}
		auto geom = a->GetAttribute("geometry");
		if (geom) {
			a = curGroup->OpenMDArray(geom->ReadAsString());
			if (a == nullptr) {
				Rcout << "could not open geometry array " << geom->ReadAsString() << std::endl;
				stop("geometry array missing");
			}
			auto nc = a->GetAttribute("node_coordinates");
			if (nc && nc->GetDataType().GetClass() == GEDTC_STRING && nc->GetDimensionCount() == 0) {
				const char *ncs = nc->ReadAsString();
				if (ncs) {
					const CPLStringList nc_names(CSLTokenizeString2(ncs, " ", 0)); // x and y coordinate array
					auto gt = a->GetAttribute("geometry_type");
					if (gt == nullptr || gt->GetDataType().GetClass() != GEDTC_STRING)
						stop("cannot get geometry_type attribute");
					auto nco = a->GetAttribute("node_count");
					auto pnco = a->GetAttribute("part_node_count");
					auto ir = a->GetAttribute("interior_ring");
					lst = List::create(
						_["geometry_type"] = CharacterVector::create(gt->ReadAsString()),
						_["x"] = get_dimension_values(curGroup->OpenMDArray(nc_names[0])),
						_["y"] = get_dimension_values(curGroup->OpenMDArray(nc_names[1])),
						_["node_count"] = nco ? get_dimension_values(curGroup->OpenMDArray(nco->ReadAsString())) : List::create(),
						_["part_node_count"] = pnco ? get_dimension_values(curGroup->OpenMDArray(pnco->ReadAsString())) : List::create(),
						_["interior_ring"] = ir ?  get_dimension_values(curGroup->OpenMDArray(ir->ReadAsString())): List::create() 
					);
				}
			} 
		}
	}
	return(lst);
}

List get_all_arrays(std::shared_ptr<GDALGroup> curGroup, List ret, std::string name) {
	auto array_names(curGroup->GetMDArrayNames());
	// for (size_t i = 0 i < array_names
	CharacterVector a(array_names.size());
	// ret needs to be a _named_ list, so that na is never null
	CharacterVector na = ret.attr("names");
	if (a.size() > 0) { // group with array(s):
		for (int i = 0; i < a.size(); i++)
			a[i] = array_names[i];
		ret.push_back(a);
		CharacterVector gn;
		// gn.push_back("");
		std::string group_name;
		if (name == "/")
			group_name = name;
		else
			group_name = name + "/";
		na.push_back(group_name);
	}
	ret.attr("names") = na;
	auto gn(curGroup->GetGroupNames());
	for (const auto &gn: curGroup->GetGroupNames()) { // iterate over groups:
		std::string slash;
		if (name == "/")
			slash = "";
		else
			slash = "/";
		ret = get_all_arrays(curGroup->OpenGroup(gn), ret, name + slash + gn);
	}
	return ret;
}

std::shared_ptr<GDALMDArray> get_array(std::shared_ptr<GDALGroup> grp, const std::string &osName) {
	CPLStringList aosTokens(CSLTokenizeString2(osName.c_str(), "/", 0));
	for (int i = 0; i < aosTokens.size() - 1; i++) {
		auto curGroupNew = grp->OpenGroup(aosTokens[i]);
		if (!curGroupNew) {
			Rcout << "Cannot find group " << aosTokens[i] << std::endl;
			stop("group not found");
		}
		grp = curGroupNew;
	}
	const char *pszArrayName = aosTokens[aosTokens.size() - 1];
	auto array(grp->OpenMDArray(pszArrayName));
	if (!array) {
		Rcout << "Cannot open array" << pszArrayName << std::endl;
		stop("array not found");
	}
	return array;
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

	if (array_names.size() == 1 && array_names[0] == "?") {
		List l;
		l.attr("names") = CharacterVector::create();
		return get_all_arrays(poRootGroup, l, poRootGroup->GetName());
	}

	auto curGroup = poRootGroup;
	// find possible vector geometry array, and construct
	List geometry = get_geometry(curGroup);

	// Rcout << "name: " << curGroup->GetName() << " full_name: " << curGroup->GetFullName() << std::endl;
	if (array_names.size() == 0) { // find the one(s) with the most dimensions:
		int ndim = 0;
		int largest_size = 0;
		for (const auto &an: curGroup->GetMDArrayNames()) { // find largest size:
			auto a(curGroup->OpenMDArray(an));
			ndim = a->GetDimensions().size();
			if (ndim > largest_size)
				largest_size = ndim;
		}
		for (const auto &an: curGroup->GetMDArrayNames()) { // identify target arrays:
			auto a(curGroup->OpenMDArray(an));
			ndim = a->GetDimensions().size();
			if (ndim == largest_size)
				array_names.push_back(an);
		}
		if (array_names.size() == 0)
			stop("no array names found");
	}
	int n = array_names.size();

	const char *name = array_names[0];
	std::shared_ptr<GDALMDArray> array;
	array = get_array(curGroup, name);
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
	for (const auto &poDim: array->GetDimensions()) {
		dim_names.push_back(poDim->GetName());
		if (offset.size() == 0)
			offst.push_back(0);
		else
			offst.push_back(offset[i]);
		if (step.size() == 0)
			stp.push_back(1);
		else
			stp.push_back(step[i]);
		if (count.size() == 0 || count[i] == NA_INTEGER || count[i] <= 0)
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
		auto arr(get_array(curGroup, name));
		dims.attr("names") = dim_names;
		dimensions.attr("names") = dim_names;
		if (! proxy) { // read the arrays:
			auto data_type(arr->GetDataType());
			size_t sz = data_type.GetSize();
			if (data_type.GetClass() == GEDTC_NUMERIC) {
				NumericVector vec(nValues);
				if (debug)
					Rcout << "size of vec: " << vec.size() << "\n";
				bool ok = arr->Read(offst.data(),
							anCount.data(),
							stp.data(), /* step: defaults to 1,1,1 */
							nullptr, /* stride: default to row-major convention */
							GDALExtendedDataType::Create(GDT_Float64),
							vec.begin());
				if (!ok)
					stop("Cannot read array into a Float64 buffer");
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
					for (size_t j = 0; j < nValues; j++) {
						if (ISNAN(vec[j]) || (has_nodata && vec[j] == nodata_value))
							vec[j] = NA_REAL;
						else
							vec[j] = vec[j] * scale + offst;
					}
				}
				vec.attr("dim") = dims;
				vec.attr("units") = arr->GetUnit();
				vec_lst[i] = vec;
			} else if (data_type.GetClass() == GEDTC_COMPOUND) {
				const auto &components = data_type.GetComponents();
				std::vector<GByte> buf(sz * nValues);
				bool ok = arr->Read(offst.data(),
							anCount.data(),
							stp.data(), /* step: defaults to 1,1,1 */
							nullptr, /* stride: default to row-major convention */
							data_type,
							&buf[0]);
				if (!ok)
					stop("Cannot read array into Compound buffer");
				DataFrame tbl;
				GByte *v = buf.data();
				for (const auto &co: components) { 
					auto t(co->GetType());
					if (t.GetClass() == GEDTC_NUMERIC) {
						if (t.GetNumericDataType() != GDT_Float64)
							stop("only Float64 data supported in numeric compounds");
						NumericVector vec(nValues);
						for (size_t j = 0; j < nValues; j++)
							memcpy(&(vec[j]), v + j * sz + co->GetOffset(), sizeof(double));
						tbl.push_back(vec, co->GetName());
					} else if (t.GetClass() == GEDTC_STRING) {
						CharacterVector vec(nValues);
						const char *str;
						for (size_t j = 0; j < nValues; j++) {
							memcpy(&str, v + j * sz + co->GetOffset(), sizeof(const char *));
							vec[j] = str; // deep copy
						}
						tbl.push_back(vec, co->GetName());
					} else 
						stop("unsupported type");
				}
				vec_lst[i] = tbl;
			} else { // GEDTC_STRING:
				std::vector<GByte> buf(sz * nValues);
				bool ok = arr->Read(offst.data(),
							anCount.data(),
							stp.data(), /* step: defaults to 1,1,1 */
							nullptr, /* stride: default to row-major convention */
							data_type,
							&buf[0]);
				if (!ok)
					stop("Cannot read array into string buffer");
				GByte *v = buf.data();
				CharacterVector vec(nValues);
				const char *str;
				for (size_t j = 0; j < nValues; j++) {
					memcpy(&str, v + j * sz, sizeof(const char *));
					vec[j] = str; // deep copy
				}
				vec.attr("dim") = dims;
				vec.attr("units") = arr->GetUnit();
				vec_lst[i] = vec;
			}
		}
	}
	vec_lst.attr("names") = a_names;
	std::shared_ptr<OGRSpatialReference> srs = array->GetSpatialRef();
	List ret = List::create(
		_["array_list"] = vec_lst,
		_["dimensions"] = dimensions,
		_["srs"] = srs == nullptr ? CharacterVector::create(NA_STRING) : wkt_from_spatial_reference(srs.get()),
		_["geometry"] = geometry
	);
	return ret;
}

/// WRITE:
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
				CharacterVector CreationOptions, bool as_float = true) {
	
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
	CharacterVector dimnames; 
	if (dimensions.attr("names") != R_NilValue)
		dimnames = dimensions.attr("names");
	else
		stop("dimensions should have names");
	std::vector<std::shared_ptr<GDALDimension>> all_dims;
	for (int i = dimensions.size() - 1; i >= 0; i--) { // backwards, for whatever reason
		std::string type;
		std::string direction = "";
		if (dimnames[i] == xy[0]) // "x"
			type = "HORIZONTAL_X";
		else if (dimnames[i] == xy[1]) // "y"
			type = "HORIZONTAL_Y";
		else if (dimnames[i] == "depth") {
			type = "VERTICAL";
			direction = "DOWN";
		} else if (dimnames[i] == "height") {
			type = "VERTICAL";
			direction = "UP";
		} else if (dimnames[i] == "time")
			type = "TEMPORAL";
		else
			type = "";
		const char *name = dimnames[i];
		std::shared_ptr<GDALDimension> d = g->CreateDimension(name, type, direction, dimensions[i], nullptr);
		if (d == nullptr)
			stop("creation of dimension failed");
		all_dims.push_back(d);
	}
	std::reverse(all_dims.begin(), all_dims.end()); // because I can't think backwards

	// create & write variables to g; write attributes
	GDALExtendedDataType edt = GDALExtendedDataType::Create(GDT_Float64);
	CharacterVector names; 
	if (variables.attr("names") != R_NilValue)
		names = variables.attr("names");
	else
		stop("variables should have names");
	LogicalVector which_crs;
	if (variables.attr("which_crs") != R_NilValue)
		which_crs = variables.attr("which_crs");
	else
		stop("which_crs attribute missing");
	LogicalVector is_numeric;
	if (variables.attr("is_numeric") != R_NilValue)
		is_numeric = variables.attr("is_numeric");
	else
		stop("is_numeric attribute missing");

	for (int i = 0; i < variables.size(); i++) {
		NumericVector a;
		CharacterVector c;
		IntegerVector which_dims;
		std::vector<std::shared_ptr<GDALDimension>> dims;
		if (is_numeric[i]) {
			if (as_float)
				edt = GDALExtendedDataType::Create(GDT_Float32);
			else
				edt = GDALExtendedDataType::Create(GDT_Float64);
			a = variables[i];
			if (a.attr("which_dims") == R_NilValue)
				stop("variable has no attribute which_dims");
			else
				which_dims = a.attr("which_dims");
		} else {
			edt = GDALExtendedDataType::CreateString(0);
			c = variables[i];
			if (c.attr("which_dims") == R_NilValue)
				stop("variable has no attribute which_dims");
			else
				which_dims = c.attr("which_dims");
		}
		for (int i = which_dims.size() - 1; i >= 0; i--) {
			if (which_dims[i] == NA_INTEGER)
				stop("NA value in which_dims: logic error");
			dims.push_back(all_dims[which_dims[i]]);
		}
		const char *name = names[i];
		std::shared_ptr<GDALMDArray> mda = g->CreateMDArray(name, dims, edt, nullptr);
		if (dims.size() == 1 && names[i] == dimnames[which_dims[0]])
			dims[0]->SetIndexingVariable(mda); // FIXME: NetCDF doesn't have?
		if (dest != NULL && which_crs[i] && !mda->SetSpatialRef(dest))
			warning("failed to assign CRS to array");

		// set start & count of writing area:
		std::vector<GUInt64> start;
		std::vector<size_t> count;
		for (int i = dims.size() - 1; i >= 0; i--) {
			start.push_back(0); // FIXME: modify if updating sub-array
			count.push_back(dimensions[which_dims[i]]);
		}
		bool success = true;
		if (is_numeric[i]) { // write numeric array:
			if (a.attr("attrs") != R_NilValue)
				write_attributes(mda, a.attr("attrs"));
			if (a.size() != 0) {
				// Rcout << "Variable: " << name << ", ndims: " << dims.size() << ", crs: " << which_crs[i] << std::endl;
				if (as_float) {
					std::vector<float> flt(a.size());
					for (int j = 0; j < a.size(); j++)
						flt[j] = a[j];
					success = mda->Write(start.data(), count.data(), nullptr, nullptr, edt, flt.data(), nullptr, 0);
				} else
					success = mda->Write(start.data(), count.data(), nullptr, nullptr, edt, &(a[0]), nullptr, 0);
			}
		} else { // write character array:
			if (c.attr("attrs") != R_NilValue)
				write_attributes(mda, c.attr("attrs"));
			if (c.size() != 0) {
				if (dims.size() != 1)
					stop("can only write one-dimensional character variables");
				std::vector<const char *> v;
				for (int i = 0; i < c.size(); i++) {
					const char *cp = c[i];
					v.push_back(cp);
				}
				success = mda->Write(start.data(), count.data(), nullptr, nullptr, edt, v.data(), nullptr, 0);
			}
		}
		if (! success)
			Rcout << "Error writing array " << name << std::endl;
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
				CharacterVector CreationOptions, bool as_float = true) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
#endif
