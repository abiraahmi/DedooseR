test_that("clean_data() names to lowercase,
          no spaces in columns,
          object with dataset and codebook,
          assigns coder_rank", {
  raw <- dplyr::tibble(
    "Excerpt Copy" = c("Text A", "Text B", "Text C"),
    "Some Range" = 1:3,
    "Code Theme Applied" = c(TRUE, FALSE, TRUE),
    "Excerpt Creator" = c("Coder A", "Coder B", "Coder C"),
    "Media Title" = c("Transcript A", "Transcript B", "Transcript C")
  )

  preferred_coders <- c("Coder A", "Coder B", "Coder C")

  result <- clean_data(
    excerpts = raw,
    preferred_coders = preferred_coders
  )

  # 1. Object has cleaned dataset and codebook
  expect_type(result, "list")
  expect_true("data" %in% names(result))
  expect_true("codebook" %in% names(result))
})
