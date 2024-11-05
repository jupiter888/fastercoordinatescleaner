# Load necessary packages
library(CoordinateCleaner)
library(FasterCoordinatesCleaner) 
library(microbenchmark)
library(ggplot2)
library(dplyr)

# Load the sample DataFrame with relevant columns
input_file_path <- "/Users/Downloads/gbif_germany (1).rds"  # Ensure path is correct
sample_data <- readRDS(input_file_path)

# Reduce the size of the sample data to avoid memory issues
sample_data <- sample_data[1:100000, ]  # Adjust the number of rows as needed

# Select only the required columns
sample_data <- sample_data[, c("decimalLongitude", "decimalLatitude", "species")]

# Define parameters for benchmarking
lon_col <- "decimalLongitude"
lat_col <- "decimalLatitude"
species_col <- "species"
outliers_method <- "quantile"
outliers_mtp <- 5
outliers_td <- 1000
outliers_size <- 7

# Function to perform benchmarking
run_benchmark <- function(value) {
  microbenchmark(
    `CoordinateCleaner::cc_outl` = {
      gc()
      CoordinateCleaner::cc_outl(sample_data, lon = lon_col, lat = lat_col, species = species_col,
                                 method = outliers_method, mltpl = outliers_mtp, tdi = outliers_td,
                                 min_occs = outliers_size, value = value)
    },
    `FasterCoordinatesCleaner::cc_outl` = {
      gc()
      FasterCoordinatesCleaner::cc_outl(sample_data, lon = lon_col, lat = lat_col, species = species_col,
                                        method = outliers_method, mltpl = outliers_mtp, tdi = outliers_td,
                                        min_occs = outliers_size, value = value, verbose = FALSE)
    },
    times = 2  # Reduced the number of iterations to save memory
  )
}

# Benchmark for "clean" value
benchmark_clean <- run_benchmark("clean")

# Benchmark for "ids" value
benchmark_ids <- run_benchmark("ids")

# Benchmark for "flagged" value
benchmark_flagged <- run_benchmark("flagged")

# Print benchmarking results
print(benchmark_clean)
print(benchmark_ids)
print(benchmark_flagged)

# Plot the results
ggplot(benchmark_clean, aes(x = expr, y = time / 1e6, fill = expr)) +  # time is in nanoseconds, convert to milliseconds
  geom_boxplot() +
  labs(title = "Benchmark: CoordinateCleaner::cc_outl vs FasterCoordinatesCleaner::cc_outl (clean)",
       x = "Function",
       y = "Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("#FF9999", "#99CCFF")) +
  geom_text(stat = "summary", fun = median, aes(label = paste0(round(..y.., 2), " ms")), vjust = -0.5)

ggplot(benchmark_ids, aes(x = expr, y = time / 1e6, fill = expr)) +  # time is in nanoseconds, convert to milliseconds
  geom_boxplot() +
  labs(title = "Benchmark: CoordinateCleaner::cc_outl vs FasterCoordinatesCleaner::cc_outl (ids)",
       x = "Function",
       y = "Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("#FF9999", "#99CCFF")) +
  geom_text(stat = "summary", fun = median, aes(label = paste0(round(..y.., 2), " ms")), vjust = -0.5)

ggplot(benchmark_flagged, aes(x = expr, y = time / 1e6, fill = expr)) +  # time is in nanoseconds, convert to milliseconds
  geom_boxplot() +
  labs(title = "Benchmark: CoordinateCleaner::cc_outl vs FasterCoordinatesCleaner::cc_outl (flagged)",
       x = "Function",
       y = "Execution Time (ms)") +
  theme_minimal() +
  scale_fill_manual(values = c("#FF9999", "#99CCFF")) +
  geom_text(stat = "summary", fun = median, aes(label = paste0(round(..y.., 2), " ms")), vjust = -0.5)

# Compare statistics summary of cleaned datasets returned
cleaned_data_coordcleaner <- CoordinateCleaner::cc_outl(sample_data, lon = lon_col, lat = lat_col, species = species_col,
                                                        method = outliers_method, mltpl = outliers_mtp, tdi = outliers_td,
                                                        min_occs = outliers_size, value = "clean")
cleaned_data_faster <- FasterCoordinatesCleaner::cc_outl(sample_data, lon = lon_col, lat = lat_col, species = species_col,
                                                         method = outliers_method, mltpl = outliers_mtp, tdi = outliers_td,
                                                         min_occs = outliers_size, value = "clean", verbose = FALSE)

# Print summary statistics for cleaned datasets
summary(cleaned_data_coordcleaner)
summary(cleaned_data_faster)
