#!/bin/bash

#SBATCH -o Output/GPUopenCLTotient15.out

echo "TESTING: 1 to 15000, WG:25"
for i in `seq 1 5`;
do
	echo "Run ${i}"
	srun --gres=gpu GPUopenclTotient 15000 25
done
