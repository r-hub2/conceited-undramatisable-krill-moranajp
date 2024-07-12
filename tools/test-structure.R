testthat::test_that("position_sentence() work", {
  x <- letters[1:10]
  y <- "a"
  expect_equal(position_sentence(x, y), 1)

  x <- letters[1:10]
  y <- "z"
  expect_equal(position_sentence(x, y), 0)

  x <- letters[10:1]
  y <- letters[c(19:10)]
  expect_equal(position_sentence(x, y), 0.1)

})

testthat::test_that("aling_sentence() work", {
  # s_id = "sentence"; term = "term"; x_pos = "x"
  s1 <- 1:4
  s2 <- 3:6
  s3 <- 5:7
  s4 <- 6:10
  s_order <- list(s1, s2, s3, s4)
  term <- purrr::map2(list(letters), s_order, `[`)
  df <- tibble::tibble(
          sentence = rep(seq_along(term), purrr::map_int(term, length)),
          term = unlist(term),
          x = seq_along(term))
  df_expect <- df |>
    dplyr::mutate(x = unlist(s_order))

  expect_equal(align_sentence(df), df_expect)
})
