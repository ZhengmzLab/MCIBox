

for F in ch*.bed;
do  
echo $F
Rscript ../SH05.mkrds.R $F
echo $F.SUBRDS
done
rename bed.SUBRDS SUBRDS.smp *.SUBRDS
ls 
cd ../



#chrX.bed.head.SUBRDS
