# test_script.R

library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

raw_data <- read_xlsx("inst/raw_data/test_data_manipulated.xlsx")

# Clean data
filepath <- "inst/raw_data/test_data_manipulated.xlsx"
preferred_coders <- c("s", "r", "l", "a")
excerpts <- clean_data(filepath = filepath, preferred_coders = preferred_coders)

# Summarize codes
summarize_codes(excerpts = excerpts, preferred_coders = preferred_coders, output_type = "datatable")

# Quality indicator check

excerpts <- read_xlsx("inst/raw_data/test_data_manipulated.xlsx")
preferred_coders <- c("s", "r", "l", "a")
quality_indicators(
  excerpts = excerpts,
  preferred_coders = preferred_coders,
  qual_indicators = c("Priority excerpt", "Heterogeniety")
)

# Plot saturation by qual indicators
plot_saturation(excerpts)

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


# Instructions: PUSH TO PACKAGE
## Create shell for function script
library(usethis)
  # REPLACE with function name
use_r("clean_data.R")

# Paste function

## Build > Check

## Git
  # Stage
  # Commit
  # Push

# Update documentation - run below in console
devtools::document()
# If you want to clear your current function so no conflict exists, run:
rm(summarize_codes)

# Run devtools::install() to rebuild & install your local package.
devtools::install()


# Restart R session and load package (library(DedooseR)).

# Test your functions
