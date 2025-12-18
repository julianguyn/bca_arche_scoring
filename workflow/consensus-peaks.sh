#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=30G
#SBATCH --time=8:00:00

#SBATCH --job-name=consensusPeak
#SBATCH --output=../logs/consensus.out

# set directories
cd scripts

cell_summit="../../data/rawdata/ATACSeq/cell_lines/summits"
pdx_summit="../../data/rawdata/ATACSeq/PDXs/summits"

cell_outdir="../../data/results/cell_lines/consensus"
pdx_outdir="../../data/results/PDXs/consensus"

module load R
Rscript consensus_corces.R $cell_summit $cell_outdir
Rscript consensus_corces.R $pdx_summit $pdx_outdir