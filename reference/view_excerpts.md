# View Qualitative Excerpts by Code

Displays qualitative excerpts interactively in a searchable, filterable
data table. Each row represents an excerpt associated with one or more
qualitative codes. Code columns are automatically detected as those
starting with `"c_"`, and their variable labels (if available) are used
as readable code names.

This function is primarily designed for exploring and reviewing coded
qualitative data, allowing users to filter by code and quickly browse
the corresponding excerpts.

## Usage

``` r
view_excerpts(data)
```

## Arguments

- data:

  A data frame containing at least one text column named `excerpt` and
  one or more logical code columns prefixed with `"c_"`. Each logical
  column represents whether a code was applied (`TRUE`/`FALSE`).

## Value

A [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) object
that displays:

- **Code:** readable code label or variable name

- **Excerpt:** associated qualitative text

The output table includes:

- A dropdown filter for selecting specific codes

- Search boxes for column-wise filtering

- Responsive column widths and formatted text wrapping

## Details

- Variable labels are extracted from the `"label"` attribute of each
  code column (e.g., assigned via
  [`haven::labelled`](https://haven.tidyverse.org/reference/labelled.html)
  or `attr(x, "label") <- "Label"`).

- Only excerpts where a code is marked as `TRUE` are displayed.

- The table uses custom styling with a purple header and automatic text
  wrapping.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

df <- tibble::tibble(
  excerpt = c(
    "I felt supported by my peers.",
    "Teachers really listened to us.",
    "I learned a lot about myself."
  ),
  c_support = c(TRUE, TRUE, FALSE),
  c_growth = c(FALSE, FALSE, TRUE)
)
attr(df$c_support, "label") <- "Peer/Teacher Support"
attr(df$c_growth, "label") <- "Personal Growth"

# View excerpts interactively
if (interactive()) view_excerpts(df)
```
