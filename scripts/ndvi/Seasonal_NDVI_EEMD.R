# This code is for the seasonal nonlinear NDVI trend analysis (1997-2024) using EEMD
# Long season: March-June
# Short season: September-December



# Libraries
library(terra)
library(Rlibeemd)
library(EMD)
library(imputeTS)


# Load supplied EEMD pixel function
source("EEMD_trend_fun.r")



# Load seasonal NDVI stacks
long_ndvi <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_long_season_stack_1997_2024.tif"
)

short_ndvi <- rast(
  "~/SeasonalTrendsEdited/data/ndvi_short_season_stack_1997_2024.tif"
)



# EEMD parameters
para <- list(
  factor = 1,
  start = c(1997, 1),
  frequency = 1,
  ensemble_size = 100,
  noise_strength = 0.2,
  S_number = 4,
  num_siftings = 50,
  fill_value = NA,
  out.list = FALSE
)



# Output layer names
years <- 1997:2024

eemd_names <- c(
  paste0("Residual_", years),
  paste0("TrendAccum_", years),
  paste0("Rate_", years),
  "trend_class",
  "break_time",
  "accumulated_before",
  "accumulated_after",
  "mean_rate_before",
  "mean_rate_after"
)



# Long season EEMD
long_eemd <- app(
  long_ndvi,
  fun = EEMD_trend_fun_at_pixel,
  para = para,
  filename = "~/SeasonalTrendsEdited/output/long_season_EEMD_full_1997_2024.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    filetype = "GTiff"
  )
)

names(long_eemd) <- eemd_names


# Short-season EEMD
short_eemd <- app(
  short_ndvi,
  fun = EEMD_trend_fun_at_pixel,
  para = para,
  filename = "~/SeasonalTrendsEdited/output/short_season_EEMD_full_1997_2024.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    filetype = "GTiff"
  )
)

names(short_eemd) <- eemd_names




# Extract EEMD trajectory classes

# 1: monotonic decrease
# 2: monotonic increase
# 3: increase to decrease
# 4: decrease to increase

long_class <- long_eemd[["trend_class"]]
short_class <- short_eemd[["trend_class"]]

class_labels <- data.frame(
  ID = 1:4,
  class = c(
    "Monotonic decrease",
    "Monotonic increase",
    "Increase to decrease",
    "Decrease to increase"
  )
)

levels(long_class) <- class_labels
levels(short_class) <- class_labels




# Save EEMD trajectory-class rasters
writeRaster(
  long_class,
  "~/SeasonalTrendsEdited/output/long_season_EEMD_trend_class.tif",
  overwrite = TRUE
)

writeRaster(
  short_class,
  "~/SeasonalTrendsEdited/output/short_season_EEMD_trend_class.tif",
  overwrite = TRUE
)




# Calculate trajectory class percentages

long_freq <- freq(long_class)
short_freq <- freq(short_class)

long_freq$percent <-
  long_freq$count / sum(long_freq$count) * 100

short_freq$percent <-
  short_freq$count / sum(short_freq$count) * 100

print(long_freq)
print(short_freq)




# EEMD trajectory maps

eemd_colors <- c(
  "red", # decrease
  "green", # increase
  "orange", # increase to decrease
  "blue" # decrease to increase
)

png(
  "~/SeasonalTrendsEdited/output/long_season_EEMD_trend_map.png",
  width = 2200,
  height = 2200,
  res = 600
)

plot(
  long_class,
  type = "classes",
  col = eemd_colors,
  main = "Long season EEMD Productivity Trend Classes (1997-2024)",
  axes = FALSE
)

dev.off()


png(
  "~/SeasonalTrendsEdited/output/short_season_EEMD_trend_map.png",
  width = 2200,
  height = 2200,
  res = 300
)

plot(
  short_class,
  type = "classes",
  col = eemd_colors,
  main = "Short season EEMD Productivity Trend Classes (1997-2024)",
  axes = FALSE
)

dev.off()