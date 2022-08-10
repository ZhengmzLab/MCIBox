#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
IN=args[1]
saveRDS(read.table(IN),paste0(IN,".SUBRDS"))
