F=human.combined.mapq-ge30.clusters.gz

mkdir $F.SUBLIB
cd $F.SUBLIB
zcat ../$F|awk -F"." -v NM=$F '{print >> NM"."$1".sublib"}'




