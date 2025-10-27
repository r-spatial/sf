# from https://zarr.readthedocs.io/en/v1.1.0/tutorial.html
import zarr
z = zarr.zeros((100, 100), chunks=(50, 50), dtype='i4')
z[:] = 1
#zarr.core.Array((10000, 10000), int32, chunks=(1000, 1000), order=C)
#  compression: blosc; compression_opts: {'clevel': 5, 'cname': 'lz4', 'shuffle': 1}
#  nbytes: 381.5M; nbytes_stored: 313; ratio: 1277955.3; initialized: 0/100
#  store: builtins.dict

# Example: persist in-memory zarr array `z` to disk
#from zarr import DirectoryStore, open as zopen
from zarr import DirectoryStore, open as zopen

store = DirectoryStore('ones.zarr')                 # directory on disk
dst = zopen(store, mode='w',
             shape=z.shape, chunks=z.chunks,
             dtype=z.dtype, compressor=getattr(z, "compressor", None),
             order=getattr(z, "order", "C"))
dst[:] = z[:]                                           # copy data
dst.attrs.update(z.attrs)                               # copy metadata (if any)

# reopen later:
z_on_disk = zopen(store, mode='r')
z_on_disk
