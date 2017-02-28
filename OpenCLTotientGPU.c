#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <CL/cl.h>
#include "simple.h"

const char *KernelSource =                                "\n"
"long sumArray(unsigned long a[], int numElements)         \n"
"{                                                         \n"
"   int i;                                                 \n"
"   unsigned long sum=0;                                   \n"
"   for (i = 0; i < numElements; i++)                      \n"
"       sum = sum + a[i];                                  \n"
"   return(sum);                                           \n"
"}                                                         \n"
"long hcf(unsigned long x, unsigned long y) {              \n"
"   long t;                                                \n"
"   while (y != 0) {                                       \n"
"       t = x % y;                                         \n"
"       x = y;                                             \n"
"       y = t;                                             \n"
"   }                                                      \n"
"   return x;                                              \n"
"}                                                         \n"
"int relprime(unsigned long x, unsigned long y) {          \n"
"   return hcf(x, y) == 1;                                 \n"
"}                                                         \n"
"long euler(unsigned long n) {                             \n"
"   unsigned long length, i;                               \n"
"   length = 0;                                            \n"
"   for (i = 1; i < n; i++)                                \n"
"       length += (relprime(n, i));                        \n"
"   return length;                                         \n"
"}                                                         \n"
"__kernel void totient(                                    \n"
"   const unsigned int lower,                              \n"
"   __local unsigned long* locRes,                         \n"
"   __global unsigned long* results)                       \n"
"{                                                         \n"
"   int i = (int)get_global_id(0);                         \n"
"   int j = (int)get_local_id(0);                          \n"
"   int lsize = (int)get_local_size(0);                    \n"
"   int groupNum = (int)get_group_id(0);                   \n"
"   locRes[j] = euler(lower + (groupNum * lsize) + j);     \n"
"   barrier(CLK_LOCAL_MEM_FENCE);                          \n"
"   if (j==0) {                                            \n"
"       results[groupNum] = sumArray(locRes, lsize);       \n"
"   }                                                      \n"
"}                                                         \n"
"__kernel void sumResults(                                 \n"
"   const unsigned int resNum,                             \n"
"   __global unsigned long* results)                       \n"
"{                                                         \n"
"   if (resNum < 4) {                                      \n"
"       results[0] = sumArray(results, resNum);            \n"
"   } else {                                               \n"
"       int j = (int)get_local_id(0);                      \n"
"       int halfSize = resNum / 2;                         \n"
"       int index = j * halfSize;                          \n"
"       for (int i = 1; i < halfSize; i++)                 \n"
"           results[index] += results[index + i];          \n"
"       barrier(CLK_LOCAL_MEM_FENCE);                      \n"
"       if (j==0) {                                        \n"
"           results[0] += results[halfSize];               \n"
"           if (halfSize * 2 != resNum)                    \n"
"               results[0] += results[resNum-1];           \n"
"       }                                                  \n"
"   }                                                      \n"
"}                                                         \n"
"\n";

struct timespec start, stop;

// hcf x 0 = x
// hcf x y = hcf y (rem x y)
long hcf(long x, long y) {
    long t;
    while (y != 0) {
        t = x % y;
        x = y;
        y = t;
    }
    return x;
}

// relprime x y = hcf x y == 1
int relprime(long x, long y) {
    return hcf(x, y) == 1;
}

// euler n = length (filter (relprime n) [1 .. n-1])
long euler(long n) {
    long length, i;
    length = 0;
    for (i = 1; i < n; i++)
        if (relprime(n, i))
            length++;
    return length;
}

// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
long sumTotient(long lower, long upper) {
    long sum, i;
    sum = 0;
    for (i = lower; i <= upper; i++) {
        long res = euler(i);
        sum = sum + res;
    }
    return sum;
}

// sumArray a numElements = sum(a)
unsigned long sumArray(unsigned long a[], int numElements)
{
    int i;
    unsigned long sum=0;
    for (i=0; i<numElements; i++)
    {
        sum = sum + a[i];
    }
    return(sum);
}

void printTimeElapsed( char *text)
{
    double elapsed = (stop.tv_sec -start.tv_sec)*1000.0
                    + (double)(stop.tv_nsec -start.tv_nsec)/1000000.0;
    printf( "%s: %f msec\n", text, elapsed);
}

void timeDirectImplementation( int lower, int upper)
{
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &start);
    long sum = sumTotient(lower, upper);
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &stop);
    printf("Result: %ld\n",sum);
    printTimeElapsed( "kernel equivalent on host");
}


int main (int argc, char * argv[])
{
    int lower;
    int upper;
    cl_int err;
    cl_kernel kernel;
    size_t global[1];
    size_t local[1];

    if (argc < 2) {
        lower = 1;
        upper = 30;
        local[0] = 10;
    } else if (argc < 3) {
        lower = 1;
        upper = atoi(argv[1]);
        local[0] = 10;
    } else if (argc < 4) {
        lower = 1;
        upper = atoi(argv[1]);
        local[0] = atoi(argv[2]);
    } else {
        lower = atoi(argv[1]);
        upper = atoi(argv[2]);
        local[0] = atoi(argv[3]);
    }

    printf( "work group size: %d\n", (int)local[0]);

    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &start);

    /* Create data for the run.    */
    int count = (upper - lower) + 1;
    global[0] = count;
    int resSize = upper/local[0];
    unsigned long result = 0;

    unsigned long *results = NULL;  /* Results returned from device.         */
    unsigned long *locres = NULL;  /* Array for summing local Results       */

    results = (unsigned long *) malloc (resSize * sizeof (unsigned long));
    locres = (unsigned long *) malloc (local[0] * sizeof (unsigned long));

    err = initGPU();

    if( err == CL_SUCCESS) {
        //Compute Euler Totient Sum and sum local groups
        kernel = setupKernel( KernelSource, "totient", 3, IntConst, lower,
                                                          LocalLongArr, local[0], locres,
                                                          LongArr, resSize, results);
        runKernel( kernel, 1, global, local);
        printKernelTime();

        //Sum local group values and store result in first value
        kernel = setupKernel( KernelSource, "sumResults", 2, IntConst, resSize,
                                                             LongArr, resSize, results);
        global[0] = 2;
        runKernel( kernel, 1, global, global);
        //result = sumArray(results, resSize);
        result = results[0];
        clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &stop);
        printf("Result: %ld\n", result);

        printKernelTime();
        printTimeElapsed( "CPU time spent");

        err = clReleaseKernel (kernel);
        err = freeDevice();

        //timeDirectImplementation( lower, upper);

    }
    return 0;
}
