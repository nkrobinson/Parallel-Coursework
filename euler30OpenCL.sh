#!/bin/bash

#SBATCH -o Output/openCLTotient30.out

echo "TESTING: 1 to 30000, WG:25"
for i in `seq 1 5`;
do
	echo "Run ${i}"
	srun --gres=gpu openclTotient 30000 25
done
