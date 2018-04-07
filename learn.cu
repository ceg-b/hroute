#include <iostream>
#include <math.h>
#include<cstdio>

#define THDIM 256
// Kernel function to add the elements of two arrays
__global__
void add(int n, float *x, float *y, float *z)
{
  int index = blockIdx.x;
  int stride = blockDim.x;
  while (index<n) {
        z[index] = x[index] + y[index];
        index+=stride;
  }
}

__global__
void sum(int n, float *x, float *z)
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





int main(void)
{
  int N = 1<<20;
  float *x, *y, *z;
  float sm=0;
  int blocks=256;

  //N=2048;
  //  N=36;

  // Allocate Unified Memory – accessible from CPU or GPU
  cudaMallocManaged(&x, N*sizeof(float));
  cudaMallocManaged(&y, N*sizeof(float));
  cudaMallocManaged(&z, N*sizeof(float));

  // initialize x and y arrays on the host
  for (int i = 0; i < N; i++) {
    x[i] = 1.0f;
    y[i] = 2.0f;
  }

  // Run kernel on 1M elements on the GPU
  //  add<<<2, 2>>>(N, x, y,z);
  sum<<<blocks,blocks>>>(N, y, z);

  // Wait for GPU to finish before accessing on host
  cudaDeviceSynchronize();

  // Check for errors (all values should be 3.0f)
  for (int i = 0; i < blocks; i++)
    sm+=z[i];

  std::cout << "Overall sum " << (sm-2*N) << std::endl;
        
        //  std::cout << "Max error: " << maxError << std::endl;

  // Free memory
  cudaFree(x);
  cudaFree(y);
  cudaFree(z);
  
  return 0;
}
