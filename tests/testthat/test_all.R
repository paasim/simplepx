context("helpers")
test_that("construct_body returns an expected result with an empty body", {
  expect_equal(as.character(construct_body(tibble())),
               '{"query":[],"response":{"format":"json"}}')
})

test_that("check_colname_comp errors with weird result", {
  col_names <- "a"
  res_json <- list(data = list(list(key = 1), list(key = c(1,2))))
  expect_error(check_colname_comp(col_names, res_json), "Unexpected")
})


context("px_nav")

# path to a dataset for testing
synt_path <- "StatFin/vrm/synt/statfin_synt_pxt_12dj.px"

test_that("px_nav works as expected with the default argument", {
  df1 <- px_nav()
  expect_true(is_tibble(df1))
  expect_true(nrow(df1) > 0)
  expect_named(df1, c("dbid", "text"))
})

test_that("px_nav works as expeted with a dataset-page", {
  res <- px_nav(synt_path)
  expect_true(is.list(res))
  expect_named(res, c("title", "variables"))
  expect_true(is_tibble(res$variables))
  expect_true(nrow(res$variables) > 0)
  cols_exp <- c("code", "text", "values", "valueTexts")
  expect_true(all(cols_exp %in% colnames(res$variables)))
})

test_that("px_nav returns an error with non-valid url", {
  expect_error(px_nav(api = "http://www.google.fi/"), "non-JSON")
})

context("px_var")
test_that("px_var works as expeted with a dataset-page", {
  res <- px_nav(synt_path)
  df1 <- px_var(synt_path)
  expect_true(is_tibble(df1))
  expect_named(df1, res$variables$text)
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

test_that("px_dl gives an error when HTTP status code is not 200", {
  expect_error(px_dl("StatFin/asu/ashi/nj/statfin_ashi_pxt_112q.px"),
               "returned with an error")
})

test_that("px_dl works as expeted with a dataset-page", {
  df0 <- px_var(synt_path)
  df1 <- px_dl(synt_path, df0)
  cols <- intersect(names(df1), names(df0))
  vals_match <- map2_lgl(select(df0, cols), select(df1, cols),
                         ~setequal(.x, .y))
  expect_true(all(vals_match))
})

test_that("px_dl works as expeted with a var-argument", {
  df0 <- px_var(synt_path) %>%
    filter(.data$Vuosi > 1990 & .data$Sukupuoli == "Miehet")
  df1 <- px_dl(synt_path, df0)
  vals_match <- select(df0, -Tiedot) %>%
    map2_lgl(select(df1, -matches("^value$")), ~setequal(.x, .y))
  expect_true(all(vals_match))
})

ene_path <- "StatFin/ene/ehi/statfin_ehi_pxt_12ge.px"
test_that("px_dl works as expeted with simplify_colnames, na_omit = TRUE", {
  var <- px_var(ene_path) %>% filter(.data$Kuukausi == "1988M06", .data$Tiedot == "Hinta")
  df1 <- px_dl(ene_path, var)
  df2 <- px_dl(ene_path, var, na_omit = TRUE)
  expect_true(nrow(df2) > 0L)
  expect_true(nrow(df2) < nrow(df1))

  df_dims <- px_dl(ene_path, var, simplify_colnames = TRUE) %>%
    colnames()
  df2_colnames_lower <- c("kuukausi", "polttoneste", "value")
  expect_identical(df_dims, df2_colnames_lower)
})

