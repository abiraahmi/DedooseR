# Test 1: recoded variables + untouched codes remain
test_that("recode() combines logical codes and preserves labels", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B", "Text C"),
    c_positive_impact = c(TRUE, FALSE, TRUE),
    c_joy = c(TRUE, FALSE, TRUE),
    c_hope = c(TRUE, FALSE, TRUE),
    c_negative_impact = c(FALSE, TRUE, FALSE),
    excerpt_creator = c("Coder A", "Coder B", "Coder C"),
    media_title = c("Transcript A", "Transcript B", "Transcript C")
  )

  recode_plan <- list(
    c_positive = c("c_positive_impact", "c_joy", "c_hope")
  )
  relabels <- list(
    c_positive = "Positive affect"
  )

  result <- recode(raw, recode_plan, relabel_vars = relabels)
  data_recode <- result[["data_recode"]]
  codebook_recode <- result[["codebook_recode"]]
  expect_identical(data_recode, result[["data_merged"]])
  expect_identical(codebook_recode, result[["codebook_merged"]])

  # 1. Result keeps original excerpt metadata and adds recoded logical codes
  expect_setequal(
    names(data_recode),
    c("excerpt_copy", "excerpt_creator", "media_title", "c_positive", "c_negative_impact")
  )
  expect_equal(nrow(data_recode), nrow(raw))

  # 2. Logical OR is applied across the requested source variables
  expect_equal(
    data_recode[["c_positive"]],
    c(TRUE, FALSE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Codes not recoded remain unchanged (both values and lack of label)
  expect_equal(
    data_recode[["c_negative_impact"]],
    raw[["c_negative_impact"]],
    ignore_attr = TRUE
  )
  expect_null(labelled::var_label(data_recode[["c_negative_impact"]]))

  # 4. Labels update: custom label applied when supplied
  expect_identical(labelled::var_label(data_recode[["c_positive"]]), "Positive affect")

  # 5. Codebook reflects updated labels and types
  codebook_positive <- dplyr::filter(codebook_recode, variable == "c_positive")
  expect_equal(codebook_positive$label, "Positive affect")
  expect_equal(codebook_positive$type, "logical")

  codebook_negative <- dplyr::filter(codebook_recode, variable == "c_negative_impact")
  expect_equal(codebook_negative$label, "c_negative_impact")
  expect_equal(codebook_negative$type, "logical")
})

# Test 2: recode() drops source variables but preserves overlapping names
test_that("recode() drops source variables but preserves overlapping names", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B"),
    c_positive = c(TRUE, FALSE),
    c_joy = c(TRUE, TRUE)
  )
  recode_plan <- list(
    c_positive = c("c_positive", "c_joy")
  )
  result <- recode(raw, recode_plan)
  data_recode <- result[["data_recode"]]

  # 1. Original source column `c_joy` is dropped after recoding
  expect_false("c_joy" %in% names(data_recode))

  # 2. Overlapping name `c_positive` remains with combined logical values
  expect_true("c_positive" %in% names(data_recode))
  expect_equal(
    data_recode[["c_positive"]],
    c(TRUE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Default label falls back to the variable name
  expect_identical(labelled::var_label(data_recode[["c_positive"]]), "c_positive")
})

test_that("recode() errors when source variables are missing", {
  raw <- dplyr::tibble(
    c_positive = c(TRUE, FALSE)
  )
  recode_plan <- list(
    c_positive = c("c_positive", "c_joy")
  )

  expect_error(
    recode(raw, recode_plan),
    "Some variables for c_positive not found in dataset",
    fixed = TRUE
  )
})
