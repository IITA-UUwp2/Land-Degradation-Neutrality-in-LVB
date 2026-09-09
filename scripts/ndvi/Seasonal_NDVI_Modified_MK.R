# Packages
library(terra)
library(modifiedmk)


# Load long-season NDVI stack (March – June)
long_ndvi_stack <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_long_season_stack_1997_2024.tif"
)


# Load short-season NDVI stack
short_ndvi_stack <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_short_season_stack_1997_2024.tif"
)


# Pixel-wise Hamed–Rao modified Mann–Kendall function
mk_fun <- function(x) {

  # Return NA if the entire pixel time series is missing
  if (all(is.na(x))) {
    return(c(
      slope = NA_real_,
      pvalue = NA_real_
    ))
  }

  # Remove missing observations
  x <- x[!is.na(x)]

  # Require a minimum number of valid observations
  if (length(x) < 10) {
    return(c(
      slope = NA_real_,
      pvalue = NA_real_
    ))
  }


  # Apply Hamed–Rao modified Mann–Kendall test. mmkh3lag accounts for serial autocorrelation. Here using the first three lags.
  mk <- try(
    suppressWarnings(
      modifiedmk::mmkh3lag(x)
    ),
    silent = TRUE
  )

  # Return NA if the test fails for a pixel
  if (inherits(mk, "try-error")) {
    return(c(
      slope = NA_real_,
      pvalue = NA_real_
    ))
  }

  # Extract outputs from mmkh3lag
  # [[7]] = Sen's slope
  # [[2]] = corrected p-value
  slope <- as.numeric(
    mk[[7]]
  )

  pvalue <- as.numeric(
    mk[[2]]
  )


  # Convert non-finite outputs such as NaN/Inf to NA
  if (!is.finite(slope)) {
    slope <- NA_real_
  }

  if (!is.finite(pvalue)) {
    pvalue <- NA_real_
  }

  # Return Sen's slope and corrected p-value
  return(c(
    slope = slope,
    pvalue = pvalue
  ))
}


# Apply modified Mann–Kendall to every pixel
long_mk <- app(
  long_ndvi_stack,
  fun = mk_fun,
  filename = "~/SeasonalTrendsEdited/output/long_season_MK_results.tif",
  overwrite = TRUE
)


# Assign descriptive layer names
names(long_mk) <- c(
  "Sen_slope",
  "p_value"
)


# Apply modified Mann–Kendall to every pixel
short_mk <- app(
  short_ndvi_stack,
  fun = mk_fun,
  filename = "~/SeasonalTrendsEdited/output/short_season_MK_results.tif",
  overwrite = TRUE
)


# Assign descriptive layer names
names(short_mk) <- c(
  "Sen_slope",
  "p_value"
)




# Classify productivity trends

# Classification (significance threshold: p < 0.05):
# 1 : significant browning
# 2 : no significant trend
# 3 : significant greening

trend_class_fun <- function(x) {

  slope <- x[1]

  pvalue <- x[2]

  # Exclude pixels without valid MK results

  if (is.na(slope) || is.na(pvalue)) {

    return(NA_real_)

  }

  # Significant negative trend

  if (pvalue < 0.05 && slope < 0) {

    return(1)

  }

  # Significant positive trend

  if (pvalue < 0.05 && slope > 0) {

    return(3)

  }

  # Remaining valid pixels are classified as having no significant trend

  return(2)

}




# Apply classification to long season

long_class <- app(
  long_mk,
  fun = trend_class_fun,
  filename = "~/SeasonalTrendsEdited/output/long_season_trend_class.tif",
  overwrite = TRUE
)

names(long_class) <- "Trend_class"



# Apply classification to short season

short_class <- app(
  short_mk,
  fun = trend_class_fun,
  filename = "~/SeasonalTrendsEdited/output/short_season_trend_class.tif",
  overwrite = TRUE
)

names(short_class) <- "Trend_class"




# Calculation of class frequencies and %s

long_freq <- freq(long_class)

short_freq <- freq(short_class)

long_freq$percentage <-
  long_freq$count / sum(long_freq$count) * 100

short_freq$percentage <-
  short_freq$count / sum(short_freq$count) * 100


cat("Long season\n")

print(long_freq)

cat("Short season\n")

print(short_freq)