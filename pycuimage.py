#!/usr/bin/env python3

# https://wiki.tiker.net/PyCuda/Examples/Convolution
import pycuda.autoinit
import pycuda.driver as drv
import numpy as np
import matplotlib.pyplot as plt
import sys
from scipy import misc
import string

from pycuda.compiler import SourceModule

CSIZE=12
THREAD=512
template= '''

//__device__ __constant__ float d_Kernel_rows[(2*CSIZE+1)*(1+CSIZE*2)];

__global__ void maxmax(int n,float *in, float* out)
{

   int stride = blockDim.x;
   int grid = gridDim.x; 
   int my_point =  blockIdx.x*blockDim.x+threadIdx.x;  

   __shared__ float cache[$THREAD];
   

   cache[threadIdx.x]=in[my_point];
   __syncthreads();
  
   while ((my_point+=grid*stride)<n) {
           if (cache[threadIdx.x]>in[my_point])
                 cache[threadIdx.x]=in[my_point];
           __syncthreads();
   }

   stride/=2;
   while (threadIdx.x<stride && stride>0) {
        if (cache[threadIdx.x]<cache[threadIdx.x+stride])
               cache[threadIdx.x]=cache[threadIdx.x+stride];
        __syncthreads();
        stride/=2;
   }

  out[blockIdx.x]=cache[0];
}

__global__ void naive_conv(int n,int m, int depth,float *in, float *out)
{
        int my_point =  blockIdx.x*blockDim.x+threadIdx.x;
        int local_row = my_point/m;
        int local_col = my_point-local_row*m;
        int ii,jj;
     
        float tmp=0;

        while (my_point<m*n) {

        local_row = my_point/m;
        local_col = my_point-local_row*m;

        int start_row = local_row-$CSIZE<0 ? 0 :  local_row-$CSIZE;
        int stop_row =  local_row+$CSIZE>=n ? n-1 :  local_row+$CSIZE;

        int start_col = local_col-$CSIZE<0 ? 0 :  local_col-$CSIZE;
        int stop_col =  local_col+$CSIZE>=m ? m-1 :  local_col+$CSIZE;

        tmp=0;
        for (ii=start_row;ii<stop_row;ii++) {
             for (jj=start_col;jj<stop_col;jj++) {
                 tmp+=in[3*(jj+m*ii)];
             }
        }
        
        //tmp/=(1024*$CSIZE*$CSIZE);
        //tmp=in[3*my_point];
        out[3*my_point]=tmp;
        out[3*my_point+1]=in[3*my_point+1]/255;
        out[3*my_point+2]=in[3*my_point+2]/255;


        my_point+=blockDim.x*gridDim.x;
        }
}
'''

template = string.Template(template)
code = template.substitute(CSIZE = CSIZE,THREAD=THREAD)
module = SourceModule(code)

naive_conv = module.get_function('naive_conv')
maxmax= module.get_function('maxmax')
if __name__ == "__main__":

    try:
        pic=sys.argv[1]
    except:
        sys.exit(1)

    
    fig = misc.imread(pic)
    gfg = fig.astype('float32')
    res = np.zeros(np.shape(gfg)).astype('float32')
    smm = np.zeros(1024).astype('float32')
    
    n,m,d = np.shape(fig)
    print(np.shape(fig))
    print(fig.dtype)
    print(gfg.dtype)

    image_on_gpu = drv.mem_alloc(n*m*d*4)
    modified_image_on_gpu = drv.mem_alloc(n*m*d*4)
    

    drv.memcpy_htod(image_on_gpu,gfg)
#    naive_conv(np.int32(n),np.int32(m),np.int32(d),drv.In(gfg),drv.Out(res),block=(1024,1,1), grid=(128,1))
    naive_conv(np.int32(n),np.int32(m),np.int32(d),image_on_gpu,modified_image_on_gpu,block=(1024,1,1), grid=(128,1))
    maxmax(np.int32(n*m*d),drv.In(res),drv.Out(smm),block=(THREAD,1,1), grid=(1024,1))

    print(np.max(smm))
    drv.memcpy_dtoh(res,modified_image_on_gpu)
    
    plt.subplot(121)
    plt.imshow(fig)        
    plt.axis('off')
    plt.subplot(122)
    plt.imshow(res)
    plt.axis('off')
    plt.show()
