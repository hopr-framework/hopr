#!/bin/bash

for f in ./*.tex; do
  pdflatex $f
done 

for f in ./*.pdf; do

	name="${f%.pdf}" 
  ext=${name##*-}
  if [[ $ext == 'crop' ]]; then
    continue
  fi

  newPDF=$name'-crop.pdf'
  newJPG=$name'.jpg'
  newPNG=$name'.png'
  pdfcrop $f
  #convert $newPDF $newJPG
  convert           \
   -verbose       \
   -density 150   \
   -trim          \
    $newPDF      \
   -quality 100   \
   -flatten       \
   -sharpen 0x1.0 \
    $newPNG
done 

rm -f *.aux *.log *-crop.pdf *.pdf
