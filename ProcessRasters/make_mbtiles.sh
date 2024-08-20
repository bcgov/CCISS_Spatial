#!/bin/bash

for file in *.tif; do
    if [ -f "$file" ]; then
        filename=$(basename -- "$file")
        filename_no_ext="${filename%.*}"
        gdal_translate $file "${filename_no_ext}.mbtiles" -of MBTILES -co TILE_FORMAT=WEBP -co QUALITY=65 -co RESAMPLING=NEAREST -co ZOOM_LEVEL_STRATEGY=UPPER
        gdaladdo "${filename_no_ext}.mbtiles" 2 4 8 16
        echo "Done $file"
    fi
done