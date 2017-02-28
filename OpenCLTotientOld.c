#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <CL/cl.h>
#include "simple.h"

const char *KernelSource =                                 "\n"
"unsigned long hcf(unsigned long x, unsigned long y) {      \n"
"    unsigned long t;                                       \n"
"    while (y != 0) {                                       \n"
"        t = x % y;                                         \n"
"        x = y;                                             \n"
"        y = t;                                             \n"
"    }                                                      \n"
"    return x;                                              \n"
"}                                                          \n"
"int relprime(unsigned long x, unsigned long y) {           \n"
"    return hcf(x, y) == 1;                                 \n"
"}                                                          \n"
"unsigned long euler(unsigned long n) {                     \n"
"    unsigned long length, i;                               \n"
"    length = 0;                                            \n"
"    for (i = 1; i < n; i++)                                \n"
"        if (relprime(n, i))                                \n"
"            length++;                                      \n"
"    return length;                                         \n"
"}                                                          \n"
"__kernel void totient(                                     \n"
"   const unsigned int lower,                               \n"
"   __global unsigned long* results)                        \n"
"{                                                          \n"
"   int i = get_global_id(0);                               \n"
"   unsigned long res = euler(i+lower);                     \n"
"   results[i] = res;                                       \n"
"}                                                          \n"
"\n";


struct timespec start, stop;

// hcf x 0 = x
// hcf x y = hcf y (rem x y)
unsigned long hcf(unsigned long x, unsigned long y) {
    unsigned long t;

    while (y != 0) {
        t = x % y;
        x = y;
        y = t;
    }
    return x;
}

// relprime x y = hcf x y == 1
int relprime(unsigned long x, unsigned long y) {
    return hcf(x, y) == 1;
}

// euler n = length (filter (relprime n) [1 .. n-1])
unsigned long euler(unsigned long n) {
    unsigned long length, i;

    length = 0;
    for (i = 1; i < n; i++)
        if (relprime(n, i))
            length++;
    return length;
}

// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
unsigned long sumTotient(unsigned long lower, unsigned long upper) {
    unsigned long sum, i;

    sum = 0;
    for (i = lower; i <= upper; i++) {
		unsigned long res = euler(i);
        sum = sum + res;
	}
    return sum;
}

unsigned long sumArray(unsigned long a[], int num_elements)
{
   unsigned long i, sum=0;
   for (i=0; i<num_elements; i++)
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
    unsigned long sum = sumTotient(lower, upper);
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
		local[0] = 32;
	} else if (argc < 3) {
		lower = 1;
		upper = atoi(argv[1]);
		local[0] = 32;
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
    unsigned long *results = NULL;  /* Results returned from device.         */
    int count = (upper - lower) + 1;
    global[0] = count;
    results = (unsigned long *) malloc (count * sizeof (unsigned long));

    err = initGPU();

    if( err == CL_SUCCESS) {
		printf("Setup Kernel\n");
        kernel = setupKernel( KernelSource, "totient", 2, IntConst, lower,
                                                          LongArr, count, results);
		printf("Run Kernel\n");
        runKernel( kernel, 1, global, local);
        unsigned long result = sumArray(results, count);
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
