#!/bin/bash

#SBATCH -o Output/openCLTotient15.out

echo "TESTING: 1 to 15000, WG:25"
for i in `seq 1 100`;
do
	echo "Run ${i}"
	srun --gres=gpu openclTotient 15000 25
done
