# #!/bin/bash

# # Extent for ROI in Kenya 

# EXTENT="29.2390747070312500 1.2642822265625000 35.8536987304687500 -4.0397949218750000"

# GDAL_OPTS="-of GTiff -co COMPRESS=deflate -co TILED=TRUE"
# BANDS="blue green red nir swir1 swir2 thermal"
# COG_DIR="."

# mkdir -p $COG_DIR

# for year in $(seq 1997 2024); do
# 	for band in $BANDS; do
# 		for dt in ${year}0101_${year}0228 ${year}0301_${year}0430 ${year}0501_${year}0630 ${year}0701_${year}0831 ${year}0901_${year}1031 ${year}1101_${year}1231; do
# 			cog_name="${band}_glad.landsat.ard2.swa_m_30m_s_${dt}_go_epsg.4326_v1.tif"
# 			echo gdal_translate --config CPL_VSIL_CURL_ALLOWED_EXTENSIONS ".tif" -projwin $EXTENT $GDAL_OPTS /vsicurl/https://s3.opengeohub.org/arco/$cog_name $COG_DIR/$cog_name
# 		done
# 	done
# done


# Extent for ROI in Kenya - Lake Victoria Basin (Kenyan part)


EXTENT="33.9097099304199219 1.2642822265625000 35.8536987304687500 -1.8880207574146295"
GDAL_OPTS="-of GTiff -co COMPRESS=deflate -co TILED=TRUE"
BANDS="red nir"
COG_DIR="."

mkdir -p $COG_DIR

for year in $(seq 1997 2024); do
    for band in $BANDS; do
        for dt in ${year}0101_${year}0228 ${year}0301_${year}0430 ${year}0501_${year}0630 ${year}0701_${year}0831 ${year}0901_${year}1031 ${year}1101_${year}1231; do
            cog_name="${band}_glad.landsat.ard2.swa_m_30m_s_${dt}_go_epsg.4326_v1.tif"
            gdal_translate --config CPL_VSIL_CURL_ALLOWED_EXTENSIONS ".tif" -projwin $EXTENT $GDAL_OPTS /vsicurl/https://s3.opengeohub.org/arco/$cog_name $COG_DIR/$cog_name
        done
    done
done