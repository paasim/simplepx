context("px_nav")
test_that("px_nav works as expected with the default argument", {
  df1 <- px_nav()
  expect_true("tbl_df" %in% class(df1))
  expect_true(nrow(df1) > 0)
  expect_identical(colnames(df1), c("dbid", "text"))
})

test_that("px_nav works as expetec with a dataset-page", {
  res <- px_nav("StatFin/vrm/synt/statfin_synt_pxt_001.px")
  expect_true(class(res) == "list")
  expect_equal(names(res), c("title", "variables"))
  expect_true("tbl_df" %in% class(res$variables))
  expect_true(nrow(res$variables) > 0)
  cols_exp <- c("code", "text", "values", "valueTexts")
  expect_true(all(cols_exp %in% colnames(res$variables)))
})

test_that("px_nav returns an error with non-valid url", {
  expect_error(px_nav(api = "http://www.google.fi/"), "non-JSON")
})

context("px_var")
test_that("px_var works as expeted with a dataset-page", {
  path <- "StatFin/vrm/synt/statfin_synt_pxt_001.px"
  res <- px_nav(path)
  df1 <- px_var(path)
  expect_true("tbl_df" %in% class(df1))
  expect_equal(colnames(df1), res$variables$text)
  rows_exp <- map_int(res$variables$valueTexts, length) %>% prod()
  expect_equal(nrow(df1), rows_exp)
  vals_match <- map2_lgl(res$variables$valueTexts, df1, ~setequal(.x, .y))
  expect_true(all(vals_match))
})

test_that("px_var gives an error with a non-dataset-page", {
  expect_error(px_var(""), "contain a dataset")
})

context("px_dl")
test_that("px_dl gives an error with a non-dataset-page", {
  expect_error(px_dl(""), "contain a dataset")
})

test_that("px_dl works as expeted with a dataset-page", {
  path <- "StatFin/vrm/synt/statfin_synt_pxt_001.px"
  df0 <- px_var(path)
  df1 <- px_dl(path)
  cols <- intersect(names(df1), names(df0))
  vals_match <- map2_lgl(select(df0, cols), select(df1, cols),
                         ~setequal(.x, .y))
  expect_true(all(vals_match))
})

test_that("px_dl works as expeted with a var-argument", {
  path <- "StatFin/vrm/synt/statfin_synt_pxt_001.px"
  df0 <- px_var(path) %>%
    filter(.data$Year > 1990 & .data$`Sex of child` == "Both sexes")
  df1 <- px_dl(path, df0)
  vals_match <- map2_lgl(df0, select(df1, -matches("^value$")),
                         ~setequal(.x, .y))
  expect_true(all(vals_match))
})

test_that("px_dl works as expeted with simplify_strings, na_omit = TRUE", {
  path <- "StatFin/ene/ehi/statfin_ehi_pxt_001_en.px"
  var <- px_var(path) %>% filter(.data$Year == 2001 & .data$Season == "Q1")
  df1 <- px_dl(path, var)
  df2 <- px_dl(path, var, na_omit = TRUE)
  expect_true(nrow(df2) < nrow(df1))

  df_dims <- px_dl(path, var, simplify_strings = TRUE) %>%
    select(-matches("^value$"))
  spaces <- map_lgl(df_dims, ~any(str_detect(.x, " ")))
  expect_false(any(spaces))

  lower <- map_df(df_dims, tolower)
  expect_identical(df_dims, lower)
})
