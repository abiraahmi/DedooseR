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
summary_data <- summarize_codes(excerpts = excerpts, preferred_coders = preferred_coders, output_type = "tibble")

# Quality indicator check
quality_indicators(
  excerpts = excerpts,
  preferred_coders = preferred_coders,
  qual_indicators = c("Priority excerpt", "Heterogeniety")
)

# Plot raw frequency
plot_counts(summary_data,
            plot_proportion = FALSE,
            min_count = 40,
            exclude_codes = c("Priority excerpt", "Heterogeniety"))


# Plot saturation by qual indicators
summary_data <- summarize_codes(excerpts, preferred_coders, output_type = "tibble")
quality_data <- quality_indicators(excerpts, preferred_coders,
                                   qual_indicators = c("Priority excerpt", "Heterogeniety"))
# Now call your plot function with these two datasets
plot_saturation(
  summary_data,
  quality_data,
  qual_indicators = c("Priority excerpt", "Heterogeniety"),
  min_counts = c("Priority excerpt" = 3, "Heterogeniety" = 3),
  stacked = TRUE,
  as_proportion = FALSE)

## Change this to compare codes when you move qual indicator criteria around
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
use_r("plot_saturation.R")

# Paste function

## Build > Check

## Git
  # Stage
  # Commit
  # Push

# Update documentation - run below in console
devtools::document()
# If you want to clear your current function so no conflict exists, run:
rm(quality_indicators)

# Run devtools::install() to rebuild & install your local package.
devtools::install()

# Update description with any changes

# Run below to rebuild sit
pkgdown::build_site()


# Restart R session and load package (library(DedooseR)).

# Test your functions
