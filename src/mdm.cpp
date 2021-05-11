// https://github.com/rouault/gdal/blob/rfc75/gdal/doc/source/tutorials/multidimensional_api_tut.rst
#include "gdal_priv.h"

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix read_mdim(CharacterVector file) {

	auto poDataset = std::unique_ptr<GDALDataset>(
		GDALDataset::Open((const char *) file[0], GDAL_OF_MULTIDIM_RASTER ));
	if( !poDataset )
		stop("file not found");

	auto poRootGroup = poDataset->GetRootGroup();
	if( !poRootGroup )
		stop("cannot open root group");

	auto poVar = poRootGroup->OpenMDArray("temperature");
	if( !poVar )
		stop("cannot open MDArray")

	size_t nValues = 1;
	std::vector<size_t> anCount;
	for( const auto poDim: poVar->GetDimensions() )
	{
		anCount.push_back(static_cast<size_t>(poDim->GetSize()));
		nValues *= anCount.back();
	}
	std::vector<double> values(nValues);
	poVar->Read(std::vector<GUInt64>{0,0,0}.data(),
				anCount.data(),
				nullptr, /* step: defaults to 1,1,1 */
				nullptr, /* stride: default to row-major convention */
				GDALExtendedDataType::Create(GDT_Float64),
				&values[0]);
	return 0;
}
