
# DedooseR 

<!-- badges: start -->
<!-- badges: end -->

**DedooseR** is an R package that connects with 
[Dedoose](https://www.dedoose.com/) to support the analysis of qualitative data.
It was built to help researchers streamline workflows and improve transparency 
and rigor in qualitative coding and analysis.

## Key Features

DedooseR currently allows you to:

- Clean data exported from Dedoose and populate a codebook
- Summarize code frquencies with one coder/media title in order of coder preference
- Plot raw frequencies or proportions of codes applied to codes, define min frequency to visualize  and/or exclude codes
- Set a saturation criteria and plot it
- Plot and play with saturation thresholds
- Produce and plot code co-occurrence heatmaps  

### Coming Soon

- Viewing and organizing excerpts in an interactive table
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

# Clean data
filepath <- read_xlsx("inst/raw_data/test_data.xlsx")
preferred_coders <- c("a", "l", "i", "r", "s", "v", "c", "n", "k")
clean_data <- clean_data(filepath, preferred_coders)
excerpts <- clean_data$data
codebook <- clean_data$codebook

# Merge codes
excerpts <- merge_codes(excerpts, list(
  c_belonging_connectedness = c(
    "c_sense_of_belonging", "c_sense_of_belonging_others", "c_sense_of_belonging_self",
    "c_sense_of_connectedness", "c_sense_of_connectedness_family",
    "c_sense_of_connectedness_peers", "c_sense_of_connectedness_school_community",
    "c_sense_of_connectedness_staff"
  ),
  c_suicide_comfort = c("c__suicide_comfort_directing_change", "c__suicide_comfort_general")
))


# Count codes
code_counts <- count_codes(excerpts,
                           min_count = 10,
                           exclude = c("c_priority_excerpt", "c_heterogeneity", "c_program_implementation_unique_value_opportunity_from_dc",
                                       "c_ripple_impact_ripple_impact_who", "c_ripple_impact_ripple_missed"),
                           output = "tibble")

# Plot codes
plot_counts <- plot_counts(code_counts,
                           exclude_codes = c("c_priority_excerpt", "c_self_efficacy"),
                           metric = "n_media_titles",
                           min_prop = 0.40)
                           
# View excerpts in filterable table
view_excerpts(excerpts)                           

# Set saturation
saturation <- set_saturation(code_counts, min_count = 10, min_prop_media_titles = 0.25)

# Plot saturation
plot_saturation <- plot_saturation(saturation)

# Compare saturation

# Define thresholds
thresholds_list <- list(
  Set1 = list(code_count = 20, prop_media_title = 0.2),
  Set2 = list(code_count = 40, prop_media_title = 0.4)
)

# Apply thresholds
comp_saturation <- compare_saturation(code_counts, excerpts, thresholds_list)

# Plot saturation comparisons
plot_saturation_comp <- plot_compare_saturation(comp_saturation, thresholds_list)

# Generate word cloud
wordcloud(excerpts, "c_knowledge_awareness", 
         max_words = 100,
         custom_stopwords = c("racall", "stuff", "everyone's"))

# Create co-occurence matrix
cooccur_matrix <- create_cooccur_matrix(excerpts, min_bold = 0.25, scale = "proportion", output = "data.frame")

# Create co-occurence network map
map_cooccur_matrix <- map_cooccur_matrix(cooccur_matrix, edge_min = 15)


```

## References
Hennink, M., & Kaiser, B. N. (2022). Sample sizes for saturation in qualitative 
research: A
systematic review of empirical tests. Social science & medicine, 292, 114523.

Small, M. L., & Calarco, J. M. (2022). Qualitative Literacy: A Guide to 
Evaluating
Ethnographic and Interview Research (1st ed.). University of California Press. 
https://doi.org/10.2307/j.ctv2vr9c4x 


