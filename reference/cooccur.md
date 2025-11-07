# Create a Code Co-occurrence Matrix and Network Plot

Builds a co-occurrence matrix showing how often qualitative codes appear
together within the same unit (e.g., transcript, document, or media
title). The function expects a coded dataset (`excerpts`) and returns
both a formatted matrix and (optionally) a network visualization. The
returned matrix can be displayed as raw counts or column-wise
proportions, whereas the network plot always reflects the underlying raw
counts.

## Usage

``` r
cooccur(
  excerpts = NULL,
  min_bold = 10,
  scale = c("count", "prop"),
  output = c("kable", "tibble", "data.frame"),
  plot = TRUE,
  edge_min = 10,
  layout = "circle",
  edge_color_low = "lightgray",
  edge_color_high = "purple",
  node_color = "lightblue",
  use_labels = FALSE,
  codebook = NULL
)
```

## Arguments

- excerpts:

  Data frame containing coded excerpts, with a column named
  `media_title` and code columns prefixed with `"c_"`.

- min_bold:

  Minimum value for bold highlighting in HTML table output (if
  `output = "kable"`). Default is `10`.

- scale:

  Whether to display raw counts (`"count"`) or column-wise conditional
  proportions (`"prop"`) in the returned matrix. The network plot always
  uses raw counts. Default is `"count"`.

- output:

  The format of the co-occurrence matrix output. One of `"kable"`,
  `"tibble"`, or `"data.frame"`. Default is `"kable"`.

- plot:

  Logical; whether to produce a network visualization. Default is
  `TRUE`.

- edge_min:

  Minimum edge weight (in counts) for displaying connections in the
  plot. Default is `10`.

- layout:

  Graph layout for network visualization (passed to
  [`ggraph::ggraph`](https://ggraph.data-imaginist.com/reference/ggraph.html)).
  Common options include `"circle"`, `"fr"`, or `"kk"`. Default is
  `"circle"`.

- edge_color_low, edge_color_high:

  Color gradient for edge weights in the plot. Default is `"lightgray"`
  to `"purple"`.

- node_color:

  Color for node points in the network plot. Default is `"lightblue"`.

- use_labels:

  Logical; if `TRUE`, replaces code variable names with descriptive
  labels from a provided `codebook`. Default is `FALSE`.

- codebook:

  Optional data frame with columns:

  - `variable`: the code variable name (e.g., `"c_family"`)

  - `label`: the descriptive name for the code (e.g.,
    `"Family Connectedness"`). Required when `use_labels = TRUE`.

## Value

A named list with two elements:

- matrix:

  A tibble, data frame, or formatted HTML table of the co-occurrence
  matrix.

- plot:

  A `ggplot` object visualizing the co-occurrence network (if
  `plot = TRUE`).

## Details

The function identifies columns beginning with `"c_"` as code variables.
It computes co-occurrences by summing pairwise intersections of codes
across all unique `media_title` units. The diagonal represents the
marginal frequencies (the number of transcripts where each code
appears).

The resulting matrix can be output as a tibble, a simple data frame, or
a formatted HTML table via
[`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html). If
`plot = TRUE`, the function also returns a network visualization of code
co-occurrences using `ggraph` and `igraph`. Edges are filtered via the
`edge_min` threshold, and nodes without any remaining connections are
removed from the plot.

## Examples

``` r
# Example 1: Basic co-occurrence matrix from excerpts
df <- data.frame(
  media_title = c("Doc1", "Doc2", "Doc3"),
  c_hope = c(1, 0, 1),
  c_family = c(1, 1, 0),
  c_school = c(0, 1, 1)
)

result <- cooccur(
  excerpts = df,
  scale = "count",
  output = "tibble",
  plot = TRUE
)

result$matrix  # Co-occurrence matrix
#> # A tibble: 3 × 4
#>   code     c_hope c_family c_school
#>   <chr>     <dbl>    <dbl>    <dbl>
#> 1 c_hope        2        1        1
#> 2 c_family      1        2        1
#> 3 c_school      1        1        2
result$plot    # Network plot


# Example 2: Use descriptive labels from a codebook and proportions in the table
codebook <- data.frame(
  variable = c("c_hope", "c_family", "c_school"),
  label = c("Hope & Optimism", "Family Connectedness", "School Belonging")
)

labeled_result <- cooccur(
  excerpts = df,
  use_labels = TRUE,
  codebook = codebook,
  scale = "prop",
  output = "kable",
  plot = TRUE
)

labeled_result$matrix
#> <table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
#> <caption>Code Co-occurrence Matrix (Within Transcript) Proportions</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">   </th>
#>    <th style="text-align:center;"> Hope &amp; Optimism </th>
#>    <th style="text-align:center;"> Family Connectedness </th>
#>    <th style="text-align:center;"> School Belonging </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> Hope &amp; Optimism </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Family Connectedness </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> School Belonging </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>    <td style="text-align:center;"> 0.500 </td>
#>    <td style="text-align:center;"> 1.000 </td>
#>   </tr>
#> </tbody>
#> </table>
labeled_result$plot

```
