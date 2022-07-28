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

// [[Rcpp::export]]
List write_mdim_old(List x, CharacterVector file, CharacterVector mdi, CharacterVector wkt, NumericVector gt) {

//	stop("not implemented yet");
	// create dataset in memory:
	GDALDriver *dr = GetGDALDriverManager()->GetDriverByName("MEM");
	if (dr == NULL)
		stop("creating memory driver failed");	

	NumericVector a = x[0];
	NumericVector dima = a.attr("dim");
	int nbands = 1;
	for (int i = 2; i < dima.size(); i++)
		nbands = nbands * dima[i];
    auto poDataset = std::unique_ptr<GDALDataset>(
					dr->Create("", dima[0], dima[1], nbands, GDT_Float64, NULL));
    if (!poDataset) {
        stop("Cannot create dataset in memory"); 
	}

	if (poDataset->RasterIO(GF_Write, 0, 0, dima[0], dima[1],
			a.begin(), dima[0], dima[1], GDT_Float64,
			nbands, NULL, 0, 0, 0, NULL) == CE_Failure)
		stop("write failure"); // #nocov
	
	if (mdi.size()) {
		CharacterVector mdi_names = mdi.attr("names");
		for (int i = 0; i < mdi.size(); i++)
			poDataset->SetMetadataItem(mdi_names[i], mdi[i]);
	}
	double *gtd = &(gt[0]);
	poDataset->SetGeoTransform(gtd);
	if (wkt.size()) {
		OGRSpatialReference sr(wkt[0]);
		poDataset->SetSpatialRef(&sr);
	}
	GDALDriver *nc = GetGDALDriverManager()->GetDriverByName("NetCDF");
	GDALDataset *out = nc->CreateCopy(file[0], poDataset.get(), false, NULL, GDALDummyProgress, NULL);
	GDALClose(out);
	return x;
}

void add_attrs(std::shared_ptr<GDALMDArray> md, CharacterVector attrs) {
	if (attrs.size() > 0) {
		CharacterVector names = attrs.attr("names");
		std::vector<GUInt64> empty;
		for (int i = 0; i < attrs.size(); i++) {
			const char *name = names[i];
			std::shared_ptr<GDALAttribute> at = md->CreateAttribute(name, empty,  GDALExtendedDataType::CreateString(0), nullptr);
			at->Write(attrs[i]);
		}
	}
}

std::shared_ptr<GDALDimension> add_dim(std::shared_ptr<GDALGroup> g, const char *name, 
				List dim, NumericVector values, size_t n) {
	std::shared_ptr<GDALDimension> d = g->CreateDimension(name, "", "", n, nullptr);
	if (values.size() != 0) {
		std::vector<std::shared_ptr<GDALDimension>> dv;
		dv.push_back(d);
		GDALExtendedDataType dbl = GDALExtendedDataType::Create(GDT_Float64);
		std::shared_ptr<GDALMDArray> md = g->CreateMDArray(name, dv, dbl, nullptr);
		GUInt64 start[1] = { 0 };
		size_t count[1];
		count[0] = values.size();
		md->Write(start, count, nullptr, nullptr, dbl, &(values[0]), nullptr, 0);
		if (values.attr("attrs") != R_NilValue)
			add_attrs(md, values.attr("attrs"));
	}
	return d;
}

std::shared_ptr<GDALMDArray> add_array(std::shared_ptr<GDALGroup> g, const char *name, NumericVector a, 
				std::vector<std::shared_ptr<GDALDimension>> dims, std::vector<size_t> count) {
	GDALExtendedDataType dbl = GDALExtendedDataType::Create(GDT_Float64);
	std::shared_ptr<GDALMDArray> mda = g->CreateMDArray(name, dims, dbl, nullptr);
	std::vector<GUInt64> start;
	for (size_t i = 0; i < count.size(); i++)
		start.push_back(0);
	if (a.size() != 0) {
		mda->Write(start.data(), count.data(), nullptr, nullptr, dbl, &(a[0]), nullptr, 0);
		if (a.attr("attrs") != R_NilValue)
			add_attrs(mda, a.attr("attrs"));
	}
	return mda;
}

// [[Rcpp::export]]
List write_mdim(CharacterVector name, CharacterVector driver, List x, List d, List e, CharacterVector wkt, CharacterVector curv) {
	if (name.size() != 1)
		stop("name should have length 1");
	if (driver.size() != 1)
		stop("driver should have length 1");
	GDALDriver *nc = GetGDALDriverManager()->GetDriverByName(driver[0]);
	if (nc == NULL)
		stop("cannot open driver");
	GDALDataset *md = nc->CreateMultiDimensional(name[0], nullptr, nullptr);
	if (md == NULL)
		stop("Cannot create MD array on driver");
	std::shared_ptr<GDALGroup> g = md->GetRootGroup();
	OGRSpatialReference *dest = NULL;
	if (wkt.size()) {
		char *cp = wkt[0];
		dest = new OGRSpatialReference; // where is this deleted?
		dest->importFromWkt((const char *) cp);
	}

	CharacterVector n = d.attr("names");
	std::vector<size_t> count;
	// add dimensions to g:
	std::vector<std::shared_ptr<GDALDimension>> dims;
	for (int i = d.size() - 1; i >= 0; i--) {
		List this_dim = d[i];
		NumericVector from = this_dim[0]; // $from
		NumericVector to = this_dim[1];   // $to
		count.push_back(to[0] - from[0] + 1);
		dims.push_back(add_dim(g, n[i], d[i], e[i], count.back()));
	}
	
	CharacterVector a = x.attr("names");
	// add data arrays to g:
	for (int i = 0; i < x.size(); i++) {
		std::shared_ptr<GDALMDArray> mda;
		if (curv.size() == 2 && (a[i] == curv[0] || a[i] == curv[1])) { // curvilinear
			std::vector<std::shared_ptr<GDALDimension>> dims12 = {dims.end() - 2, dims.end()};
			std::vector<size_t> count12 = {count.end() - 2, count.end()};
			// Rcout << "dims12 length: " << dims12.size() << std::endl;
			mda = add_array(g, a[i], x[i], dims12, count12);
		} else
			mda = add_array(g, a[i], x[i], dims, count);
		if (dest && (i == 0 || i == 1)) {
			if (! mda->SetSpatialRef(dest))
				warning("failed to assign CRS to array");
		}
	}
	GDALClose(md);
	return x;
}

#else
List read_mdim(CharacterVector file, CharacterVector array_names, CharacterVector oo,
				IntegerVector offset, IntegerVector count, IntegerVector step, 
				bool proxy = false, bool debug = false) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
List write_mdim(CharacterVector name, CharacterVector driver, List x, List d, List e, CharacterVector wkt, CharacterVector curv) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
List write_mdim_old(List x, CharacterVector file, CharacterVector mdi, CharacterVector wkt, NumericVector gt) {
	stop("requires GDAL >= 3.1.0 and 64-bit");
}
#endif
