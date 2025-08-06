# test_script.R

library(DedooseR)
library(tidyverse)
library(dplyr)
library(readxl)

raw_data <- read_xlsx("inst/raw_data/test_data_manipulated.xlsx")

# Clean data
filepath <- "/Users/abishankar/Library/CloudStorage/Box-Box/Directing Change: Qualitative interviews (Spring 2025)/Data/Analysis/Downloaded data/Dedoose excerpts/2025-08-06.xlsx"
preferred_coders <- c("s", "r", "l", "a")
excerpts <- clean_data(filepath = filepath, preferred_coders = preferred_coders)

# Summarize codes
df_all_summary <- summarize_codes(excerpts = excerpts, preferred_coders = preferred_coders, output_type = "tibble")

# Quality indicator check
df_qual_summary <- quality_indicators(
  excerpts = excerpts,
  preferred_coders = preferred_coders,
  qual_indicators = c("Priority excerpt", "Heterogeniety")
)

# Plot raw frequency
plot_counts(df_all_summary,
            plot_proportion = FALSE,
            min_count = 10,
            exclude_codes = c("Priority excerpt", "Heterogeniety"))


# Plot saturation by qual indicators
plot_saturation(
  df_all_summary,
  df_qual_summary,
  qual_indicators = c("Priority excerpt", "Heterogeniety"),
  min_counts = c("Priority excerpt" = 3, "Heterogeniety" = 3),
  stacked = TRUE,
  as_proportion = FALSE)

# Plot saturation comparison
# Define thresholds
thresholds_list <- list(
  "Set 1" = list(
    `Priority excerpt` = 2,
    Heterogeniety = 3
  ),
  "Set 2" = list(
    `Priority excerpt` = 5,
    Heterogeniety = 3
  )
)

plot_saturation_comp(
  df_all_summary,
  df_qual_summary,
  thresholds_list = thresholds_list,
  stacked = TRUE,
  as_proportion = TRUE,
  ncol = 2
)

# Code co-occurence

file_path <- "inst/raw_data/coccur.xlsx"
coccur(file_path, sheet = 1, min_frequency = 10)

# View excerpts
view_excerpts(excerpts)

# Instructions: PUSH TO PACKAGE
## Create shell for function script
library(usethis)
  # REPLACE with function name
use_r("summarize_codes.R")

# Paste function

## Build > Check

## Git
  # Stage
  # Commit
  # Push

# Update documentation - run below in console
devtools::document()
# If you want to clear your current function so no conflict exists, run:
rm(plot_saturation)

# Run devtools::install() to rebuild & install your local package.
devtools::install()

# Update description with any changes

# Run below to rebuild sit
pkgdown::build_site()

# Restart R session and load package (library(DedooseR)).

# Test your functions
