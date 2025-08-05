
# DedooseR 

<!-- badges: start -->
<!-- badges: end -->

**DedooseR** is an R package that connects with 
[Dedoose](https://www.dedoose.com/) to support the analysis of qualitative data.
It was built to help researchers streamline workflows and improve transparency 
and rigor in qualitative coding and analysis.

## Key Features

DedooseR currently supports:

- Summarize codes by coder
- Cleaning data exported from Dedoose and calculating code frequencies  
- Setting saturation criteria  
- Plotting saturation according to set critera
- Producing code co-occurrence heatmaps  

### Coming Soon

- Viewing and organizing excerpts in an interactive table
- Developing codebooks  
- Calculating reliability per coder (Cohen's kappa) and between coders 
(Fleiss' kappa)

## Why This Package?

Ongoing challenges in qualitative research include defining what constitutes 
high-quality data and demonstrating transparency in how saturation is 
reached (Small & Calarco, 2022). Informed by guidelines for high-quality 
qualitative research (Hennink & Kaiser, 2022), DedooseR pays particular 
attention to indicators like:

- The **concreteness** of excerpts (priority code)
- The **heterogeneity** within codes  (heterogeneity code)

By tagging these indicators in Dedoose and exploring them in R, 
we were able to gain greater confidence in both the **depth** and **diversity** 
of our dataset.

Note: while the package currently requires that concreteness and heterogeneity
are tagged in Dedoose, we will soon be relaxing the functionality so you can
use the package even without it (though we strongly recommend it for reasons 
stated above).

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

# Load excerpts and co-occurence datasets from Dedoose
excerpts <- read_xlsx("file path")
cooccurence <- read_xlsx("file path")

# Summarize codes per coder
summarize_codes(excerpts, preferred_coders, output_type = kable)

# Clean data and calculate code frequencies
long_codes <- create_saturation_tracking(excerpts,
                                         preferred_coders = 
                                         c("s", "r", "l", "a"))
                                         
# Plot code frequencies
plot_saturation(excerpts)

# Set saturation criteria
set_saturation(excerpts, min_priority = 10, min_heterogeneity = 10, plot = FALSE)

# Produce code co-occurence heat map
cooccur(cooccur, min_frequency = 10)

```

## References
Hennink, M., & Kaiser, B. N. (2022). Sample sizes for saturation in qualitative 
research: A
systematic review of empirical tests. Social science & medicine, 292, 114523.

Small, M. L., & Calarco, J. M. (2022). Qualitative Literacy: A Guide to 
Evaluating
Ethnographic and Interview Research (1st ed.). University of California Press. 
https://doi.org/10.2307/j.ctv2vr9c4x 


