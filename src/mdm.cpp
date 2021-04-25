// https://github.com/rouault/gdal/blob/rfc75/gdal/doc/source/tutorials/multidimensional_api_tut.rst
#include "gdal_priv.h"
int main()
{
    GDALAllRegister();
    auto poDataset = std::unique_ptr<GDALDataset>(
        GDALDataset::Open( "in.nc", GDAL_OF_MULTIDIM_RASTER ));
    if( !poDataset )
    {
        exit(1);
    }
    auto poRootGroup = poDataset->GetRootGroup();
    if( !poRootGroup )
    {
        exit(1);
    }
    auto poVar = poRootGroup->OpenMDArray("temperature");
    if( !poVar )
    {
        exit(1);
    }
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
