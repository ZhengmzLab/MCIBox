
echo "input a RGN file"
read FRGN

rm -rf ${FRGN}.SUB
mkdir ${FRGN}.SUB
cd ${FRGN}.SUB
cat ../$FRGN |awk -v OFS="\t" '{print $1,$2,$3,$4 >  $1".bed"}'