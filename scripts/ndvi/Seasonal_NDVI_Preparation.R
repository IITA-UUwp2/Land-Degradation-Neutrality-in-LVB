library(terra)

# Directories
output_dir <- "clipped_output"
ndvi_dir <- "D:/ndvi_output_seasonal"
temp_dir <- "D:/temp_terra"

dir.create(
  ndvi_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  temp_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

terraOptions(
  tempdir = temp_dir
)


# List all clipped tif files
clipped_files <- list.files(
  output_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Separate Red bands from NIR bands
red_files <- clipped_files[
  grepl("^red_", basename(clipped_files))
]

nir_files <- clipped_files[
  grepl("^nir_", basename(clipped_files))
]

# Check the number of available Red and NIR rasters
cat(
  "Red files:", length(red_files), "\n",
  "NIR files:", length(nir_files), "\n"
)


# Match Red and NIR rasters by bimonthly period.
# The "red_" and "nir_" prefixes are removed so the remaining
# filename can be used as a common period identifier.

get_period_key <- function(path) {

  sub(
    "^(red|nir)_",
    "",
    basename(path)
  )
}

red_keys <- sapply(
  red_files,
  get_period_key
)

nir_keys <- sapply(
  nir_files,
  get_period_key
)

# Match each Red raster with the NIR raster from the same period
pairs <- merge(

  data.frame(
    key = red_keys,
    red = red_files,
    stringsAsFactors = FALSE
  ),

  data.frame(
    key = nir_keys,
    nir = nir_files,
    stringsAsFactors = FALSE
  ),

  by = "key"
)

cat(
  "Matched Red/NIR pairs:",
  nrow(pairs),
  "\n"
)


# Extract date, year and starting month

# Extract the first YYYYMMDD date contained in each filename.
# This represents the start date of each bimonthly composite.
extract_start_date <- function(key) {

  date_string <- regmatches(
    key,
    regexpr("[0-9]{8}", key)
  )

  as.Date(
    date_string,
    format = "%Y%m%d"
  )
}

pairs$period_start <- extract_start_date(
  pairs$key
)

# Extract year
pairs$year <- as.numeric(
  format(
    pairs$period_start,
    "%Y"
  )
)

# Extract the starting month of each bimonthly period:
# 1  = Jan-Feb
# 3  = Mar-Apr
# 5  = May-Jun
# 7  = Jul-Aug
# 9  = Sep-Oct
# 11 = Nov-Dec
pairs$month <- as.numeric(
  format(
    pairs$period_start,
    "%m"
  )
)

# Arrange all matched periods chronologically
pairs <- pairs[
  order(pairs$period_start),
]


# Create a list to store the NDVI raster for every bimonthly period
ndvi_stack_list <- vector(
  mode = "list",
  length = nrow(pairs)
)

for (i in seq_len(nrow(pairs))) {

  # Load corresponding Red and NIR rasters
  red_r <- rast(
    pairs$red[i]
  )

  nir_r <- rast(
    pairs$nir[i]
  )

  # Remove invalid/non-positive reflectance values
  red_r[
    red_r <= 0
  ] <- NA

  nir_r[
    nir_r <= 0
  ] <- NA

  # Calculate NDVI
  ndvi_r <- (
    nir_r - red_r
  ) / (
    nir_r + red_r
  )

  # Define output filename for the bimonthly NDVI raster
  ndvi_file <- file.path(
    ndvi_dir,
    paste0(
      "ndvi_",
      pairs$key[i]
    )
  )

  # Save the bimonthly NDVI raster
  writeRaster(
    ndvi_r,
    ndvi_file,
    overwrite = TRUE
  )

  # Keep the raster in the list for seasonal aggregation
  ndvi_stack_list[[i]] <- ndvi_r

  cat(
    "Finished NDVI:",
    pairs$year[i],
    pairs$month[i],
    "\n"
  )
}


# Yearly long- and short-season NDVI rasters

years <- sort(
  unique(pairs$year)
)

long_files <- character(
  length(years)
)

short_files <- character(
  length(years)
)


for (i in seq_along(years)) {

  yr <- years[i]


  # Long season: March-June
  # The available Landsat products are bimonthly composites. Therefore, the long season is represented exactly by:
  # March-April + May-June

  long_idx <- which(
    pairs$year == yr &
      pairs$month %in% c(3, 5)
  )

  long_stack <- rast(
    ndvi_stack_list[long_idx]
  )

  # Sum the two bimonthly NDVI composites
  long_sum <- sum(
    long_stack,
    na.rm = TRUE
  )

  long_files[i] <- file.path(
    ndvi_dir,
    paste0(
      "ndvi_long_",
      yr,
      ".tif"
    )
  )

  writeRaster(
    long_sum,
    long_files[i],
    overwrite = TRUE
  )


  # Short season: September-December
  # # The intended short rains season includes October-December. # However, October is contained within the Sep-Oct composite and cannot be separated from September.
  # September-October + November-December

  short_idx <- which(
    pairs$year == yr &
      pairs$month %in% c(9, 11)
  )

  short_stack <- rast(
    ndvi_stack_list[short_idx]
  )

  # Sum the two bimonthly NDVI composites
  short_sum <- sum(
    short_stack,
    na.rm = TRUE
  )

  short_files[i] <- file.path(
    ndvi_dir,
    paste0(
      "ndvi_short_",
      yr,
      ".tif"
    )
  )

  writeRaster(
    short_sum,
    short_files[i],
    overwrite = TRUE
  )
}


# Stack the 28 long season rasters in chronological order
long_ndvi_stack <- rast(
  long_files
)

names(
  long_ndvi_stack
) <- years

writeRaster(
  long_ndvi_stack,
  file.path(
    ndvi_dir,
    "ndvi_long_season_stack_1997_2024.tif"
  ),
  overwrite = TRUE
)


# Stack the 28 short season rasters in chronological order
short_ndvi_stack <- rast(
  short_files
)

names(
  short_ndvi_stack
) <- years

writeRaster(
  short_ndvi_stack,
  file.path(
    ndvi_dir,
    "ndvi_short_season_stack_1997_2024.tif"
  ),
  overwrite = TRUE
)