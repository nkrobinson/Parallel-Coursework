#!/bin/bash

#SBATCH -o Output/openCLTotient100.out

echo "TESTING: 1 to 100000, WG:25"
for i in `seq 1 5`;
do
	echo "Run ${i}"
	srun --gres=gpu openclTotient 100000 25
done
