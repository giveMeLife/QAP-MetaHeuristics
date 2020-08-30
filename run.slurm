#!/bin/bash
#SBATCH -J metaheuristic
#SBATCH -p general
#SBATCH -n 1
#SBATCH --array=1-101%101
#SBATCH --output=/home/jarredondo/projects/QAP/logs/pipes/%a_%A.txt
#SBATCH --error=/home/jarredondo/projects/QAP/logs/pipes/%a_%A.err
#SBATCH --mail-user=javier.arredondo.c@usach.cl
#SBATCH --mail-type=ALL


ml R/4.0.0

heuristic='SA'
file='esc64a.txt'
workdir='/home/jarredondo/projects/QAP/'
Rscript --vanilla main.R $file 'SA' ${SLURM_ARRAY_TASK_ID} $workdir
#Rscript --vanilla main.R $file 'evolutive' ${SLURM_ARRAY_TASK_ID} $workdir
