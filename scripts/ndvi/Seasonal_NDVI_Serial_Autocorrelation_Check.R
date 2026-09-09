# Packages
library(terra)


# Load seasonal NDVI stacks
long_ndvi_stack <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_long_season_stack_1997_2024.tif"
)

short_ndvi_stack <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_short_season_stack_1997_2024.tif"
)


# Calculate basin-wide mean NDVI for each year
long_mean <- global(
  long_ndvi_stack,
  "mean",
  na.rm = TRUE
)[, 1]

short_mean <- global(
  short_ndvi_stack,
  "mean",
  na.rm = TRUE
)[, 1]


# Calculate autocorrelation for lags 1 to 3
long_acf <- acf(
  long_mean,
  lag.max = 3,
  plot = FALSE
)$acf[2:4]

short_acf <- acf(
  short_mean,
  lag.max = 3,
  plot = FALSE
)$acf[2:4]


# Approximate 95% confidence limit
n <- length(long_mean) # same number of observations

acf_limit <- 1.96 / sqrt(n)


# Summarize ACF results
acf_results <- data.frame(
  lag = 1:3,
  long_season = as.numeric(long_acf),
  short_season = as.numeric(short_acf)
)

acf_results$long_significant <-
  abs(acf_results$long_season) > acf_limit

acf_results$short_significant <-
  abs(acf_results$short_season) > acf_limit


# Display results
print(acf_limit)
print(acf_results)