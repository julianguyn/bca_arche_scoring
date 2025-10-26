#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=30G
#SBATCH --time=4:00:00

#SBATCH --job-name=consensusPeak
#SBATCH --output=../logs/consensus.out

# set directories
cd scripts

summit="../../data/rawdata/PDXs/summits"
outdir="../../data/results/PDXs/consensus"

module load R
Rscript consensus_corces.R $summit $outdir