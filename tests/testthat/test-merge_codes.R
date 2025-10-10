# Test 1: merged variables + untouched codes remain
test_that("merge_codes() merges logical codes and preserves labels", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B", "Text C"),
    c_positive_impact = c(TRUE, FALSE, TRUE),
    c_joy = c(TRUE, FALSE, TRUE),
    c_hope = c(TRUE, FALSE, TRUE),
    c_negative_impact = c(FALSE, TRUE, FALSE),
    excerpt_creator = c("Coder A", "Coder B", "Coder C"),
    media_title = c("Transcript A", "Transcript B", "Transcript C")
  )

  merges <- list(
    c_positive = c("c_positive_impact", "c_joy", "c_hope")
  )
  relabels <- list(
    c_positive = "Positive affect"
  )

  result <- merge_codes(raw, merges, relabel_vars = relabels)
  data_merged <- result[["data_merged"]]
  codebook_merged <- result[["codebook_merged"]]

  # 1. Result keeps original excerpt metadata and adds merged logical codes
  expect_setequal(
    names(data_merged),
    c("excerpt_copy", "excerpt_creator", "media_title", "c_positive", "c_negative_impact")
  )
  expect_equal(nrow(data_merged), nrow(raw))

  # 2. Logical OR is applied across the requested source variables
  expect_equal(
    data_merged[["c_positive"]],
    c(TRUE, FALSE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Codes not merged remain unchanged (both values and lack of label)
  expect_equal(
    data_merged[["c_negative_impact"]],
    raw[["c_negative_impact"]],
    ignore_attr = TRUE
  )
  expect_null(labelled::var_label(data_merged[["c_negative_impact"]]))

  # 4. Labels update: custom label applied when supplied
  expect_identical(labelled::var_label(data_merged[["c_positive"]]), "Positive affect")

  # 5. Codebook reflects updated labels and types
  codebook_positive <- dplyr::filter(codebook_merged, variable == "c_positive")
  expect_equal(codebook_positive$label, "Positive affect")
  expect_equal(codebook_positive$type, "logical")

  codebook_negative <- dplyr::filter(codebook_merged, variable == "c_negative_impact")
  expect_equal(codebook_negative$label, "c_negative_impact")
  expect_equal(codebook_negative$type, "logical")
})

# Test 2: Merge_codes errors when a source variable is missing
test_that("merge_codes() drops source variables but preserves overlapping names", {
  raw <- dplyr::tibble(
    excerpt_copy = c("Text A", "Text B"),
    c_positive = c(TRUE, FALSE),
    c_joy = c(TRUE, TRUE)
  )
  merges <- list(
    c_positive = c("c_positive", "c_joy")
  )
  result <- merge_codes(raw, merges)
  data_merged <- result[["data_merged"]]

  # 1. Original source column `c_joy` is dropped after merging
  expect_false("c_joy" %in% names(data_merged))

  # 2. Overlapping name `c_positive` remains with merged logical values
  expect_true("c_positive" %in% names(data_merged))
  expect_equal(
    data_merged[["c_positive"]],
    c(TRUE, TRUE),
    ignore_attr = TRUE
  )

  # 3. Default label falls back to the variable name
  expect_identical(labelled::var_label(data_merged[["c_positive"]]), "c_positive")
})

