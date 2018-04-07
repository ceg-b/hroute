#!/usr/bin/env python3

import pycuda.autoinit
import pycuda.driver as drv
import numpy

from pycuda.compiler import SourceModule
mod = SourceModule("""
__global__ void sumprod(int n,float *x, float *z)
{
  int index = blockIdx.x;
  int stride = blockDim.x;
  int thread = threadIdx.x;
  int grid = gridDim.x;

  __shared__ float cache[THDIM];

  //index=thread+THDIM*index;

  //printf("Hello thread %d %d %d %d\n", index,stride,thread,grid);

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

a = numpy.random.randn(400).astype(numpy.float32)
b = numpy.random.randn(400).astype(numpy.float32)

dest = numpy.zeros_like(a)
multiply_them(
        drv.Out(dest), drv.In(a), drv.In(b),
        block=(512,1,1), grid=(128,1))

print(b)

