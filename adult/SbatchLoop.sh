# !/bin/bash

for Index_loop in {1..296};
do
	sbatch ruiqi.script $Index_loop;
	echo 'Submitted times = ' $Index_loop;
done