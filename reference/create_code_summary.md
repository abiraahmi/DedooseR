# Create a Summary Table and Plot of Code Frequencies

Summarizes how often each qualitative code (represented by logical 0/1
variables) appears across excerpts or media titles. Optionally produces
a frequency table and visualization of code distributions.

This function automatically handles Stata-labelled (`haven_labelled`) or
numeric 0/1 variables by converting them to logicals. You can also pass
in a custom codebook to apply human-readable code labels.

## Usage

``` r
create_code_summary(
  excerpts,
  table_min_count = 1,
  table_min_prop = NULL,
  plot = FALSE,
  plot_min_count = NULL,
  plot_min_prop = NULL,
  output_type = c("tibble", "kable", "datatable"),
  exclude = NULL,
  plot_metric = c("count", "prop", "both"),
  fill_color = "steelblue",
  use_labels = FALSE,
  codebook = NULL
)
```

## Arguments

- excerpts:

  A data frame containing at least one logical or 0/1 variable
  representing a code, and a column named `media_title` that identifies
  the source document or excerpt.

- table_min_count:

  Minimum number of excerpts required for a code to appear in the
  summary table. Default is 1.

- table_min_prop:

  Optional proportion threshold (relative to the maximum count) for
  including codes in the table. Default is `NULL`.

- plot:

  Logical; whether to generate a plot visualizing code frequencies.
  Default is `FALSE`.

- plot_min_count:

  Minimum number of excerpts required for a code to appear in the plot.
  Defaults to `table_min_count`.

- plot_min_prop:

  Optional proportion threshold (relative to the maximum count) for
  including codes in the plot. Defaults to `table_min_prop`.

- output_type:

  The format for the output table. One of `"tibble"`, `"kable"`, or
  `"datatable"`. Default is `"tibble"`.

- exclude:

  Optional character vector of variable names to exclude from analysis.

- plot_metric:

  The metric to visualize. One of `"count"`, `"prop"`, or `"both"`.
  Default is `"count"`.

- fill_color:

  Color for plot bars. Default is `"steelblue"`.

- use_labels:

  Logical; if `TRUE`, uses a supplied `codebook` to display descriptive
  labels for codes instead of variable names. Default is `FALSE`.

- codebook:

  Optional data frame with two columns:

  - `variable`: the variable names in the dataset

  - `label`: the corresponding human-readable label for each code.
    Required when `use_labels = TRUE`.

## Value

If `plot = FALSE`, returns a table in the selected `output_type` format.
If `plot = TRUE`, invisibly returns a list with two elements:

- table:

  A table of summarized code frequencies.

- plot:

  A `ggplot` object visualizing the results.

## Details

The function first identifies all logical (or 0/1 numeric) columns in
`excerpts` and calculates:

- `count`: total number of excerpts where the code is applied

- `n_media_titles`: number of distinct media titles containing the code

- `prop_media_titles`: proportion of media titles containing the code
  (relative to max)

The table can be output as a tibble, formatted table
([`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html)), or
interactive data table
([`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html)).

When `plot = TRUE`, the function generates a ggplot2 bar chart showing
either code counts, proportions, or both (dual-axis view).

## Examples

``` r
# Example 1: Basic usage without a codebook
df <- data.frame(
  media_title = c("Doc1", "Doc2", "Doc3", "Doc4"),
  code_a = c(TRUE, FALSE, TRUE, TRUE),
  code_b = c(FALSE, TRUE, TRUE, FALSE)
)

create_code_summary(df, plot = TRUE)
#> # A tibble: 2 × 4
#>   code   count n_media_titles prop_media_titles
#>   <chr>  <int>          <int>             <dbl>
#> 1 code_a     3              3              1   
#> 2 code_b     2              2              0.67

# Example 2: Using a codebook for readable labels
codebook <- data.frame(
  variable = c("code_a", "code_b"),
  label = c("Community Engagement", "Policy Support")
)

create_code_summary(
  df,
  use_labels = TRUE,
  codebook = codebook,
  plot = TRUE,
  plot_metric = "both"
)
#> # A tibble: 2 × 4
#>   code                 count n_media_titles prop_media_titles
#>   <chr>                <int>          <int>             <dbl>
#> 1 Community Engagement     3              3              1   
#> 2 Policy Support           2              2              0.67

# Example 3: Excluding a code and outputting as datatable
create_code_summary(
  df,
  exclude = "code_b",
  output_type = "datatable"
)
```
