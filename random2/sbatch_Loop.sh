# !/bin/bash

for Index_loop in {1..200};
do
	sbatch R_quanah.sh $Index_loop;
	echo 'Submitted times = ' $Index_loop;
done