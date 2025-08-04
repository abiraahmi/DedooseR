# test_script.R

library(DedooseR)
library(dplyr)
library(tidyverse)
library(readxl)

raw_data <- read_xlsx("inst/data/test_data_manipulated.xlsx")

long_codes <- create_saturation_tracking("inst/data/test_data_manipulated.xlsx",
                                         preferred_coders = c("s", "r", "l", "a"))

# Plot saturation
plot_saturation(long_codes)

# Test returning filtered data frame only
filtered_df <- set_saturation(long_codes, min_priority = 3, min_heterogeneity = 3, plot = FALSE)
print(filtered_df)

# Test returning the plot
set_saturation(long_codes, min_priority = 3, min_heterogeneity = 3, plot = TRUE)

# Setting saturation criteria
SAT <- set_saturation(long_codes, min_priority = 10, min_heterogeneity = 10, plot = FALSE)

# Code co-occurence

file_path <- "inst/data/cooccur.xlsx"
plot_code_cooccurrence_heatmap(file_path, min_frequency = 10)


