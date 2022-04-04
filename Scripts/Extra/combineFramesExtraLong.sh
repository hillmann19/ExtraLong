DIR='/Users/hillmann/Projects/ExtraLong/data/ExtraLongFrames'
Scans=`ls ${DIR}`
for frame in ${Scans}; do
    cd ${DIR}/${frame} 
    frame_list=`ls ${DIR}/${frame}| sed 's/.png//'`
    for slice in ${frame_list};do
	if [ ${frame} == 'Euler400' -o  ${frame} == 'Euler500' ];then
	     convert ${slice}.png -crop 300x400+450+0 ${slice}cropped.png
	else
	     convert ${slice}.png -crop 450x500+950+120 ${slice}cropped.png
	fi
    done
    frame_combine=`ls ${DIR}/${frame} | grep "cropped"`
    convert ${frame_combine} +append ${frame}.png
done




