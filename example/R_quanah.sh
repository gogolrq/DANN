#!/bin/bash
#SBATCH --chdir=./
#SBATCH --job-name=example_knn
#SBATCH --output=./report/report_%j.txt
#SBATCB –-error=./report/err_%j.txt
#SBATCH --partition quanah
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=10:00:00


cd /home/ruiqliu/DKNN/revision/example

#Load the latest version of the R language - compiled using the Intel compilers.
module load intel R

#Allow R to perform some automatic parallelization.
#	MKL_NUM_THREADS - The maximum number of threads you want R to spawn on your behalf.
#	$NSLOTS - This will be replaced by the number of slots you request in yout parallel environment.
#		Example:  -pe sm 36 -> $NSLOTS=36.
#export MKL_NUM_THREADS=$NSLOTS

#Run the example R script using the Rscript application.
#for t in 0.001 0.004 0.007  0.01 0.02 0.03 0.04 0.05 0.1 0.15 0.2 0.25 0.3
#for n in 200 500 1000 2000 5000 10000

RandomSeed=$1

for n in 10000 20000
do
  for t in 0.5 0.6
  do
    for kap in 0.8
    do
      Rscript ./R_main.R $n $t $kap $RandomSeed;
    done
  done
done



