#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <CL/cl.h>
#include "simple.h"

const char *KernelSource =                        "\n"
"__kernel void totient(                            \n"
"   const unsigned int lower,                      \n"
"   const unsigned int upper,                      \n"
"   __global float* output)                        \n"
"{                                                 \n"
"   int i = get_global_id(0) + lower;              \n"
"       output[i] = euler(i);                      \n"
"}                                                 \n"
"long euler(long n) {                              \n"
"    long length, i;                               \n"
"    length = 0;                                   \n"
"    for (i = 1; i < n; i++)                       \n"
"        if (relprime(n, i))                       \n"
"            length++;                             \n"
"    return length;                                \n"
"}                                                 \n"
"long hcf(long x, long y) {                        \n"
"    long t;                                       \n"
"    while (y != 0) {                              \n"
"        t = x % y;                                \n"
"        x = y;                                    \n"
"        y = t;                                    \n"
"    }                                             \n"
"    return x;                                     \n"
"}                                                 \n"
"int relprime(long x, long y) {                    \n"
"    return hcf(x, y) == 1;                        \n"
"}                                                 \n"
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
    for (i = lower; i <= upper; i++)
        sum = sum + euler(i);
    return sum;
}

void printTimeElapsed( char *text)
{
    double elapsed = (stop.tv_sec -start.tv_sec)*1000.0
                    + (double)(stop.tv_nsec -start.tv_nsec)/1000000.0;
    printf( "%s: %f msec\n", text, elapsed);
}

void timeDirectImplementation( int lower, int upper, float* results)
{
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &start);
    sumTotient(lower, upper, results);
    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &stop);
    printTimeElapsed( "kernel equivalent on host");
}


int main (int argc, char * argv[])
{
    int lower = argv[1];
    int upper = argv[2];

    cl_int err;
    cl_kernel kernel;
    size_t global[1];
    size_t local[1];

    if( argc <4) {
        local[0] = 32;
    } else {
        local[0] = atoi(argv[3]);
    }

    printf( "work group size: %d\n", (int)local[0]);

    clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &start);

    /* Create data for the run.    */
    float *results = NULL;  /* Results returned from device.         */
    //int correct;            /* Number of correct results returned.   */

    int count = upper - lower;
    global[0] = count;

    results = (float *) malloc (count * sizeof (float));

    /* Fill the vector with random float values.    */
    for (int i = 0; i < count; i++)
        resuts[i] = 0;

    err = initGPU();

    if( err == CL_SUCCESS) {
        kernel = setupKernel( KernelSource, "totient", 3, IntConst, lower,
                                                          IntConst, upper,
                                                          FloatArr, count, results);

        runKernel( kernel, 1, global, local);

        clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &stop);

        printKernelTime();
        printTimeElapsed( "CPU time spent");

        /* Validate our results.    */
        //correct = 0;
        //for (int i = 0; i < count; i++)
        //    if (results[i] == data[i] * data[i])
        //        correct++;

        /* Print a brief summary detailing the results.    */
        //printf ("Computed %d/%d %2.0f%% correct values\n", correct, count,
        //                (float)count/correct*100.f);

        err = clReleaseKernel (kernel);
        err = freeDevice();

        timeDirectImplementation( lower, upper, results);

    }


    return 0;
}
