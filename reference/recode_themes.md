# Recode logical code variables and optionally relabel them

`recode_themes()` combines multiple logical (TRUE/FALSE) code variables
into new composite variables. For each new variable, the function
computes a logical OR across the specified source variables—meaning the
new variable is `TRUE` when *any* source variable is `TRUE`. Optionally,
descriptive labels can be supplied for the newly created variables, and
a codebook summarizing the resulting dataset is returned.

## Usage

``` r
recode_themes(data, recodes, relabel_vars = NULL)

# S3 method for class 'data.frame'
recode_themes(data, recodes, relabel_vars = NULL)

# S3 method for class 'tbl_df'
recode_themes(data, recodes, relabel_vars = NULL)

# Default S3 method
recode_themes(data, recodes, relabel_vars = NULL)
```

## Arguments

- data:

  A data frame, tibble, or haven-labelled data frame (for example, the
  output from
  [`clean_data()`](https://abiraahmi.github.io/DedooseR/reference/clean_data.md)
  or a dataset read from a `.dta` file) containing logical code
  variables.

- recodes:

  A named list where each name is a new variable to create and each
  value is a character vector of existing variable names to combine. For
  example:
  `list(c_help = c("c_support", "c_assist"), c_stress = c("c_anxiety", "c_pressure"))`

- relabel_vars:

  Optional named list of variable labels for the new composite variables
  in the format `list(new_var = "New variable label")`. If omitted, the
  new variable names are used as default labels.

## Value

A list with four elements:

- data_recode:

  A data frame containing the updated dataset with recoded logical code
  variables.

- codebook_recode:

  A data frame summarizing variable names, labels (if available), and
  data types.

- data_merged:

  Alias for `data_recode` retained for backward compatibility.

- codebook_merged:

  Alias for `codebook_recode` retained for backward compatibility.

## Details

The function first verifies that the specified source variables exist in
the dataset. It then creates the new logical variables defined by
`recodes`, assigns user-specified or default labels, removes the
original source variables (unless one overlaps with a new variable
name), and builds a codebook summarizing the recoded dataset.

## Examples

``` r
# Example dataset
df <- data.frame(
  c_support = c(TRUE, FALSE, TRUE),
  c_assist = c(FALSE, TRUE, TRUE),
  c_anxiety = c(TRUE, FALSE, FALSE),
  c_pressure = c(FALSE, TRUE, FALSE)
)

# Define recodes
recode_plan <- list(
  c_help = c("c_support", "c_assist"),
  c_stress = c("c_anxiety", "c_pressure")
)

# Run recode_themes() with new labels
result <- recode_themes(
  data = df,
  recodes = recode_plan,
  relabel_vars = list(
    c_help = "Mentions of helping or supporting others",
    c_stress = "Mentions of stress or pressure"
  )
)

# Extract recoded data and codebook
data_recode <- result$data_recode
codebook_recode <- result$codebook_recode

# View recoded dataset
head(data_recode)
#>   c_help c_stress
#> 1   TRUE     TRUE
#> 2   TRUE     TRUE
#> 3   TRUE    FALSE

# View codebook
head(codebook_recode)
#>          variable                                    label    type
#> c_help     c_help Mentions of helping or supporting others logical
#> c_stress c_stress           Mentions of stress or pressure logical
```
