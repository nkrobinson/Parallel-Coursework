#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#include <CL/cl.h>
#include "simple.h"

typedef struct {
  clarg_type arg_t;
  cl_mem dev_buf;
  float *host_buf;
  long *host_bufl;
  int    num_elems;
  int    val;
} kernel_arg;

#define MAX_ARG 10


#define die(msg, ...) do {                      \
  (void) fprintf (stderr, msg, ## __VA_ARGS__); \
  (void) fprintf (stderr, "\n");                \
} while (0)

/* global setup */

static cl_platform_id cpPlatform;     /* openCL platform.  */
static cl_device_id device_id;        /* Compute device id.  */
static cl_context context;            /* Compute context.  */
static cl_command_queue commands;     /* Compute command queue.  */
static cl_program program;            /* Compute program.  */
static int num_kernel_args;
static kernel_arg kernel_args[MAX_ARG];

static struct timespec start, stop;


cl_int initDevice ( int devType)
{
  cl_int err = CL_SUCCESS;

  /* Connect to a compute device.  */
  err = clGetPlatformIDs (1, &cpPlatform, NULL);
  if (CL_SUCCESS != err) {
    die ("Error: Failed to find a platform!");
  } else {
    /* Get a device of the appropriate type.  */
    err = clGetDeviceIDs (cpPlatform, devType, 1, &device_id, NULL);
    if (CL_SUCCESS != err) {
      die ("Error: Failed to create a device group!");
    } else {
      /* Create a compute context.  */
      context = clCreateContext (0, 1, &device_id, NULL, NULL, &err);
      if (!context || err != CL_SUCCESS) {
        die ("Error: Failed to create a compute context!");
      } else {
        /* Create a command commands.  */
        commands = clCreateCommandQueue (context, device_id, 0, &err);
        if (!commands || err != CL_SUCCESS) {
          die ("Error: Failed to create a command commands!");
        }
      }
    }
  }

 return err;
}

cl_int initCPU ()
{
  return initDevice( CL_DEVICE_TYPE_CPU);
}

cl_int initGPU ()
{
  return initDevice( CL_DEVICE_TYPE_GPU);
}

cl_kernel setupKernel( const char *kernel_source, char *kernel_name, int num_args, ...)
{
  cl_kernel kernel = NULL;
  cl_int err = CL_SUCCESS;
  va_list ap;
  int i;

  /* Create the compute program from the source buffer.  */
  program = clCreateProgramWithSource (context, 1,
                                       (const char **) &kernel_source,
                                       NULL, &err);
  if (!program || err != CL_SUCCESS) {
    die ("Error: Failed to create compute program!");
  }

  /* Build the program executable.  */
  err = clBuildProgram (program, 0, NULL, NULL, NULL, NULL);
  if (err != CL_SUCCESS)
    {
      size_t len;
      char buffer[2048];

      clGetProgramBuildInfo (program, device_id, CL_PROGRAM_BUILD_LOG,
                             sizeof (buffer), buffer, &len);
      die ("Error: Failed to build program executable!\n%s", buffer);
    }

  /* Create the compute kernel in the program.  */
  kernel = clCreateKernel (program, kernel_name, &err);
  if (!kernel || err != CL_SUCCESS) {
    die ("Error: Failed to create compute kernel!");
    kernel = NULL;
  } else {

    num_kernel_args = num_args;
    va_start(ap, num_args);
    for(i=0; (i<num_args) && (kernel != NULL); i++) {
      kernel_args[i].arg_t =va_arg(ap, clarg_type);
      switch( kernel_args[i].arg_t) {
        case LongArr:
          kernel_args[i].num_elems = va_arg(ap, int);
          kernel_args[i].host_bufl = va_arg(ap, long *);
          /* Create the device memory vector  */
          kernel_args[i].dev_buf = clCreateBuffer (context, CL_MEM_READ_WRITE,
                                                   sizeof (long) * kernel_args[i].num_elems, NULL, NULL);
		  
		  //printf("LongArr dev_buf: %ld \n", kernel_args[i].dev_buf);
          
		  if (!kernel_args[i].dev_buf ) {
            die ("Error: Failed to allocate device memory for arg %d!", i+1);
            kernel = NULL;
          } else {
            err = clEnqueueWriteBuffer( commands, kernel_args[i].dev_buf, CL_TRUE, 0,
                                                  sizeof (long) * kernel_args[i].num_elems,
                                                  kernel_args[i].host_bufl, 0, NULL, NULL);
            if( CL_SUCCESS != err) {
              die ("Error: Failed to write to source array for arg %d!", i+1);
              kernel = NULL;
            }
            err = clSetKernelArg (kernel, i, sizeof (cl_mem), &kernel_args[i].dev_buf);
            if( CL_SUCCESS != err) {
              die ("Error: Failed to set kernel arg %d!", i);
              kernel = NULL;
            }
          }
          break;
        case FloatArr:
          kernel_args[i].num_elems = va_arg(ap, int);
          kernel_args[i].host_buf = va_arg(ap, float *);
          /* Create the device memory vector  */
          kernel_args[i].dev_buf = clCreateBuffer (context, CL_MEM_READ_WRITE,
                                                   sizeof (float) * kernel_args[i].num_elems, NULL, NULL);
          if (!kernel_args[i].dev_buf ) {
            die ("Error: Failed to allocate device memory for arg %d!", i+1);
            kernel = NULL;
          } else {
            err = clEnqueueWriteBuffer( commands, kernel_args[i].dev_buf, CL_TRUE, 0,
                                                  sizeof (float) * kernel_args[i].num_elems,
                                                  kernel_args[i].host_buf, 0, NULL, NULL);
            if( CL_SUCCESS != err) {
              die ("Error: Failed to write to source array for arg %d!", i+1);
              kernel = NULL;
            }
            err = clSetKernelArg (kernel, i, sizeof (cl_mem), &kernel_args[i].dev_buf);
            if( CL_SUCCESS != err) {
              die ("Error: Failed to set kernel arg %d!", i);
              kernel = NULL;
            }
          }
          break;
        case IntConst:
          kernel_args[i].val = va_arg(ap, unsigned int);
          err = clSetKernelArg (kernel, i, sizeof (unsigned int), &kernel_args[i].val);
          if( CL_SUCCESS != err) {
            die ("Error: Failed to set kernel arg %d!", i);
            kernel = NULL;
          }
          break;
        default:
          die ("Error: illegal argument tag for executeKernel!");
          kernel = NULL;
      }
    }
  }
  va_end(ap);

  return kernel;
}

cl_int runKernel( cl_kernel kernel, int dim, size_t *global, size_t *local)
{
  cl_int err;

  clock_gettime( CLOCK_REALTIME, &start);
  int result = clEnqueueNDRangeKernel (commands, kernel, dim, NULL, global, local, 0, NULL, NULL);
  printf("RUNKERNEL-RESULT: %d\n", result);
  if (CL_SUCCESS != result) {
    die ("Error: Failed to execute kernel!");
  }

  /* Wait for all commands to complete.  */
  err = clFinish (commands);
  clock_gettime( CLOCK_REALTIME, &stop);

  for( int i=0; i< num_kernel_args; i++) {
    if( kernel_args[i].arg_t == FloatArr) {
      err = clEnqueueReadBuffer (commands, kernel_args[i].dev_buf,
                              CL_TRUE, 0, sizeof (float) * kernel_args[i].num_elems,
                              kernel_args[i].host_buf, 0, NULL, NULL);
      if( err != CL_SUCCESS)
        die( "FloatArr Error: Failed to transfer back arg %d!", i);
    }
    if( kernel_args[i].arg_t == LongArr) {
      err = clEnqueueReadBuffer (commands, kernel_args[i].dev_buf,
                              CL_TRUE, 0, sizeof (long) * kernel_args[i].num_elems,
                              kernel_args[i].host_bufl, 0, NULL, NULL);
	  //printf("LongArr kernel_args[i].num_elems %ld \n", kernel_args[i].num_elems);

      if( err != CL_SUCCESS){
		printf("LongArr kernel_args[i].num_elems %ld \n", (sizeof (long) * kernel_args[i].num_elems)/8);
		printf("LongArr Transfer GPU -> Host error: %d \n", err);
        die( "LongArr Error: Failed to transfer back arg %d!", i);
	  }
    }
  }

  return err;
}

void printKernelTime()
{
  double elapsed = (stop.tv_sec -start.tv_sec)*1000.0
                  + (stop.tv_nsec -start.tv_nsec)/1000000.0;
  printf( "time spent on kernel: %f msec\n", elapsed);
}

cl_int freeDevice()
{
  cl_int err;

  for( int i=0; i< num_kernel_args; i++) {
    if( kernel_args[i].arg_t == FloatArr)
      err = clReleaseMemObject (kernel_args[i].dev_buf);
    if( kernel_args[i].arg_t == LongArr)
      err = clReleaseMemObject (kernel_args[i].dev_buf);
  }
  err = clReleaseProgram (program);
  err = clReleaseCommandQueue (commands);
  err = clReleaseContext (context);

  return err;
}
