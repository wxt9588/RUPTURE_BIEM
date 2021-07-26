#!/bin/sh

# Useage: qsub serial.sh
# Output: <job_name>.o<job_id>

#$ -cwd
#$ -m beas
#$ -j y
#$ -S /bin/sh

######### give job a name
#$ -N job_FZ

date1=$(date +%s)
echo "Begin computing, please have a rest ..."
#########  run program_name
#`pwd`/program_name
./FZ.out
echo "finished"
date2=$(date +%s)
date3=$(($date2-$date1))
date4=$(($date3/60/60))
date5=$(($(($date3-$date4*60*60))/60))
echo "Running time : $date4 hours and $date5 minutes."

# example: run my_test.m (matlab script)
#
# cd `pwd`
# matlab -nojvm -nodisplay -r my_test

