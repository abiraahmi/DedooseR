# Clean and prepare qualitative excerpts for export

This function standardizes and cleans a dataset of qualitative excerpts
coded by multiple coders. It standardizes column names, filters excerpts
by a preferred coder hierarchy, converts code columns to logical
(TRUE/FALSE), assigns descriptive variable labels, and optionally
exports the cleaned data to Excel (`.xlsx`) or Stata (`.dta`) format.
The function also returns a codebook containing variable names, labels,
and data types.

## Usage

``` r
clean_data(
  excerpts,
  preferred_coders,
  rename_vars = NULL,
  relabel_vars = NULL,
  output_path = NULL,
  output_type = c("none", "xlsx", "dta")
)
```

## Arguments

- excerpts:

  A data frame containing excerpt-level data exported from Dedoose or a
  similar coding platform.

- preferred_coders:

  A character vector of coder names in order of preference. The function
  keeps the highest-preference coder for each unique `media_title`.

- rename_vars:

  An optional named list or
  [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)-style
  mapping of variables to rename. For example,
  `list(new_name = "old_name")`.

- relabel_vars:

  An optional named list of new variable labels. For example,
  `list(old_name = "New label for var1", var2 = "Updated label for var2")`.

- output_path:

  Optional file path to save the cleaned dataset. If `NULL`, the data
  will not be saved to disk.

- output_type:

  A string specifying the export format. Must be one of:

  - `"none"` – no file is written (default)

  - `"xlsx"` – save as Excel file via
    [`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html)

  - `"dta"` – save as Stata file via
    [`haven::write_dta()`](https://haven.tidyverse.org/reference/read_dta.html)

## Value

A list with two elements:

- `data`:

  A cleaned data frame with standardized names, filtered coders, and
  labelled variables.

- `codebook`:

  A data frame with columns: `variable`, `label`, and `type`.

## Details

The function performs the following steps:

1.  Standardizes variable names (lowercase, underscores instead of
    spaces).

2.  Renames `excerpt_copy` to `excerpt` if present.

3.  Removes columns ending with `"range"` or `"weight"`.

4.  Detects code columns matching the pattern `"^code.*applied$"` and
    converts them to logicals.

5.  Renames code columns with a `c_` prefix and assigns human-readable
    variable labels.

6.  Filters to the preferred coder per `media_title`.

7.  Applies default labels to key metadata variables (e.g.,
    `excerpt_creator`, `media_title`).

8.  Optionally renames or relabels variables via user-supplied
    arguments.

9.  Drops columns that are entirely `NA`.

10. Generates a codebook summarizing variables, labels, and types.

When exporting to `.dta`, logicals remain stored as `TRUE`/`FALSE`
rather than being coerced to 0/1. Variable labels are preserved in Stata
format using the `labelled` and `haven` packages.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- clean_data(
  excerpts = excerpts_raw,
  preferred_coders = c("CoderA", "CoderB"),
  rename_vars = list(new_name = "old_name"),
  relabel_vars = list(old_name = "new variable label"),
  output_path = "cleaned_excerpts.dta",
  output_type = "dta"
)

# Access cleaned data and codebook
head(result$data)
head(result$codebook)
} # }
```
