#!/usr/bin/env python3

import pycuda.autoinit
import pycuda.driver as drv
import numpy

from pycuda.compiler import SourceModule
mod = SourceModule("""
__global__ void multiply_them(float *dest, float *a, float *b)
{
  const int i = threadIdx.x;
  dest[i] = a[i] * b[i];
}




__global__
void sum(int n, float *x, float *z)
{
  int index = blockIdx.x;
  int stride = blockDim.x;
  int thread = threadIdx.x;
  int grid = gridDim.x;

  __shared__ float cache[512];

  index = thread+stride*index;
  
  cache[thread]=x[index];

  __syncthreads();
  while ((index+=grid*stride)<n) {
        cache[thread]+=x[index];
        __syncthreads();
  }

  stride/=2;
  while (thread<stride && stride>0) {
        cache[thread]+=cache[thread+stride];
        __syncthreads();
        stride/=2;
  }

  z[blockIdx.x]=cache[0];
}





""")

multiply_them = mod.get_function("multiply_them")
prd = mod.get_function("sum")

a = numpy.random.randn(400).astype(numpy.float32)
b = numpy.random.randn(400).astype(numpy.float32)

dest = numpy.zeros_like(a)
multiply_them(
        drv.Out(dest), drv.In(a), drv.In(b),
        block=(400,1,1), grid=(1,1))

print(dest-a*b)

prd(numpy.int32(400), drv.In(a), drv.Out(b),block=(64,1,1),grid=(128,1,1))
print(b)

ats=drv.Device(0).get_attributes()

for i in ats:
    print ("%s:\t%d\n" % (i,ats[i]))
