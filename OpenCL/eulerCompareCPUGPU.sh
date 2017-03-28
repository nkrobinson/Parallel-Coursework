#!/bin/bash

#SBATCH -o Output/compareGPUCPU.out

echo "TESTING: 1 to 100000, WG:25"
for i in `seq 1 10`;
do
	echo "Run ${i}"
	echo "CPU: \n"
	srun --gres=gpu openclTotient 100000 25
	echo "\n\nGPU: \n"
	srun --gres=gpu GPUopenclTotient 100000 25
	echo "\n\n"
done
