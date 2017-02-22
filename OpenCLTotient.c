#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <CL/cl.h>
#include "simple.h"

const char *KernelSource =                                 "\n"
"long sumArray(long a[], int num_elements)					\n"
"{															\n"
"   long i, sum=0;											\n"
"   for (i=0; i<num_elements; i++)							\n"
"   {														\n"
"	 sum = sum + a[i];										\n"
"   }														\n"
"   return(sum);											\n"
"} 															\n"
"long hcf(long x, long y) {                                 \n"
"    long t;                                                \n"
"    while (y != 0) {                                       \n"
"        t = x % y;                                         \n"
"        x = y;                                             \n"
"        y = t;                                             \n"
"    }                                                      \n"
"    return x;                                              \n"
"}                                                          \n"
"int relprime(long x, long y) {                             \n"
"    return hcf(x, y) == 1;                                 \n"
"}                                                          \n"
"long euler(long n) {                                       \n"
"    long length, i;                                        \n"
"    length = 0;                                            \n"
"    for (i = 1; i < n; i++)                                \n"
"        if (relprime(n, i))                                \n"
"            length++;                                      \n"
"    return length;                                         \n"
"}                                                          \n"
"__kernel void totient(                                     \n"
"   const unsigned int lower,                               \n"
"   __global long* results)                                 \n"
"{                                                          \n"
"	__local long* locres;									\n"
"   int i = get_global_id(0);								\n"
"   int j = get_local_id(0);								\n"
"	int lsize = (int)get_local_size(0);						\n"
"   locres[j] = euler(i+lower);                           	\n"
"	if (j==0)												\n"
"		results[i/lsize] = sumArray(locres, lsize);			\n"
"}                                                          \n"
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

long sumArray(long a[], int num_elements)
{
   long i, sum=0;
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
		local[0] = hcf(upper, 100);
	} else if (argc < 3) {
		lower = 1;
		upper = atoi(argv[1]);
		local[0] = hcf(upper, 100);
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
    long *results = NULL;  /* Results returned from device.         */
    int count = (upper - lower) + 1;
    global[0] = count;
    int resSize = upper/local[0];
    results = (long *) malloc (resSize * sizeof (long));

    /* Fill the vector with random float values.    */
    for (int i = 0; i < resSize; i++)
        results[i] = -1;

    err = initGPU();

    if( err == CL_SUCCESS) {
		printf("Setup Kernel\n");
        kernel = setupKernel( KernelSource, "totient", 2, IntConst, lower,
                                                          LongArr, resSize, results);
		printf("Run Kernel\n");
        runKernel( kernel, 1, global, local);
        long result = sumArray(results, resSize);
        clock_gettime( CLOCK_PROCESS_CPUTIME_ID, &stop);
        printf("Result: %ld\n", result);
        for (int i = 0; i < resSize; i++) {
			printf("results[%d]: %ld\n", i, results[i]);
			//if (results[i == -1])
			//	break;
		}

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

        //timeDirectImplementation( lower, upper);

    }
    return 0;
}
