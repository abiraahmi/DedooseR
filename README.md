
# DedooseR 

<!-- badges: start -->
<!-- badges: end -->

**DedooseR** is an R package that connects with 
[Dedoose](https://www.dedoose.com/) to support the analysis of qualitative data.
It was built to help researchers streamline workflows and improve transparency 
and rigor in qualitative coding and analysis.

## Key Features

DedooseR currently allows you to:

- Clean data exported from Dedoose
- Summarize code frquencies with one coder/media title in order of coder preference
- Plot raw frequencies or proportions of codes applied to total codes, define min frequency to visualize  and/or exclude codes
- Set a saturation criteria and plot it
- Plot and play with saturation criteria sets 
- Produce code co-occurrence heatmaps  

### Coming Soon

- Viewing and organizing excerpts in an interactive table
- Developing codebooks  
- Calculating reliability per coder (Cohen's kappa) and between coders 
(Fleiss' kappa)

## Why This Package?

Ongoing challenges in qualitative research include defining what constitutes 
high-quality data and demonstrating transparency in how saturation is 
reached (Small & Calarco, 2022). Informed by guidelines for high-quality 
qualitative research (Hennink & Kaiser, 2022), DedooseR allows you to
better understand your data with quality tags in Dedoose like:

- The **concreteness** of excerpts (priority code)
- The **heterogeneity** within codes  (heterogeneity code)

By tagging these indicators in Dedoose and exploring them in R, 
this allows for gain greater confidence in both the **depth** and **diversity** 
of datasets.

## Installation

You can install the development version of DedooseR from 
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("abiraahmi/DedooseR")
```

## Example

This is a basic example which shows you how you may the package:

``` r
library(DedooseR)

# Load excerpts from Dedoose
excerpts <- read_xlsx("file path")
cooccurence <- read_xlsx("file path")

# Clean data
filepath <- "path/to/your/file"
preferred_coders <- c("s", "r", "l", "a")
excerpts <- clean_data(filepath = filepath, preferred_coders = preferred_coders)

# Summarize codes (1 coder/transcript according to listed coder preference)
df_all_summary <-summarize_codes(excerpts, 
                              preferred_coders, 
                              output_type = "datatable")

# Plot counts by raq frequency, proportion of total codes, set minimum count to 
# visualize, and/or exclude codes
plot_counts(df_all_summary,
            plot_proportion = FALSE,
            min_count = 40,
            exclude_codes = c("Priority excerpt", "Heterogeniety"))


# If you've been tagging excerpts by quality indicators, set them below and 
# summarize code counts based on these selections

df_qual_summary <- quality_indicators(excerpts, 
                                  preferred_coders,
                                  qual_indicators = 
                                  c("Priority excerpt", "Heterogeniety"))
df_all_summary <- summarize_codes(excerpts, 
                                preferred_coders, 
                                output_type = "tibble")
# Now call your plot function with these two datasets
# You can set a min code frequency to plot or plot by proportion of total counts
plot_saturation(
  df_all_summary,
  df_qual_summary,
  qual_indicators = c("Priority excerpt", "Heterogeniety"),
  min_counts = c("Priority excerpt" = 3, "Heterogeniety" = 3),
  stacked = TRUE,
  as_proportion = FALSE)
  
# Play around with the thresholds and see which codes reach saturation
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
  summary_data = summary_data,
  quality_summary = quality_summary,
  thresholds_list = thresholds_list,
  stacked = TRUE,
  as_proportion = TRUE,
  ncol = 2
)

# Load co-occurence datasets from Dedoose
cooccurence <- read_xlsx("file path")
# Produce code co-occurence heat map
cooccur(cooccurence, min_frequency = 10)

```

## References
Hennink, M., & Kaiser, B. N. (2022). Sample sizes for saturation in qualitative 
research: A
systematic review of empirical tests. Social science & medicine, 292, 114523.

Small, M. L., & Calarco, J. M. (2022). Qualitative Literacy: A Guide to 
Evaluating
Ethnographic and Interview Research (1st ed.). University of California Press. 
https://doi.org/10.2307/j.ctv2vr9c4x 


