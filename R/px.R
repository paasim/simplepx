#' Download data from the PX-Web API of Statistics Finland
#'
#' `px_nav` is used to navigate the API: it returns the possible
#'  subdirectories as a tibble with dbid corresponding to the suffix in the url
#'  that points to the given subdirectory.
#'
#'  `px_var` returns the values of the dimensions in the given table and
#'  `px_dl` returns the actual data corresponding to variables specified
#'  as the `var` argument. The format of the `var` argument is the
#'  same as in the tibble returned by `px_var`, so the values can be
#'  selected by filtering that tibble. See the examples for mode information.
#'
#' @name doc-all
#' @param path suffix to the path after `api`.
#'  For `px_download` and `px_var` this must point to an actual
#'  dataset (ie. an url ending in .px), whereas for `px_nav` this should
#'  correspond to a partial path to a dataset (ie. an url that points to a
#'  directory). Defaults to `""`.
#' @param var The values of the dimensions to be queried as a tibble. Can be
#'  obtained by filtering from the result of `px_var`. If not specified,
#'  px_var is called automatically.
#' @param simplify_colnames If `TRUE`, the colnames are transformed to
#'  ascii, lowercase and spaces are replaced with underscores.
#' @param na_omit If `TRUE`, all missing values are omitted.
#'  Defaults to `FALSE`.
#' @param api The url to the API. Defaults to
#'  `"https://statfin.stat.fi/PXWeb/api/v1/fi/"`, the Finnish PX-Web API of
#'  Statistics Finland.
#'
#' @return `px_dl` and `px_var` always return a tibble.
#'  `px_nav` returns a tibble when request is sent to a non-table URL
#'  (ie. an url corresponds to a directory rather than a particular data set).
#'
#' @examples
#' # Navigate through the API
#' # px_nav()
#' # check what data is available under StatFin
#' # px_nav("StatFin/")
#' # navigate through the directories,
#' # check what variables are available for population statistics
#' # var <- px_var("StatFin/vrm/synt/statfin_synt_pxt_12dj.px",
#' #               "https://statfin.stat.fi/PXWeb/api/v1/en/")
#' # select years after 1900
#' # var_1900 <- dplyr::filter(var, Year >= 1900)
#' # Download the data, starting from year 1900,
#' # omitting the var-argument would download the data for all the years.
#' # data <- px_dl("StatFin/vrm/synt/statfin_synt_pxt_12dj.px",
#' #               var = var_1900, simplify_colnames = TRUE,
#' #               api = "https://statfin.stat.fi/PXWeb/api/v1/en/")
#'

#' @rdname doc-all
#' @export
#'
px_nav <- function(
  path = "",
  api = "https://statfin.stat.fi/PXWeb/api/v1/fi/"
) {
  url <- str_c(api, path)
  res <- GET(url)

  handle_req_errors(res)
  handle_http_errors(res)

  res_json <- content(res, "text", "application/json", "UTF-8") |> fromJSON()

  if (is.data.frame(res_json)) {
    as_tibble(res_json, .name_repair = "unique")
  } else {
    modify_if(res_json, is.data.frame, as_tibble, .name_repair = "unique")
  }
}

#' @rdname doc-all
#' @export
#'
px_var <- function(path, api = "https://statfin.stat.fi/PXWeb/api/v1/fi/") {
  res <- px_nav(path, api)

  if (!("variables" %in% names(res))) {
    stop("The page does not appear to contain a dataset.")
  }

  set_names(res$variables$valueTexts, res$variables$text) |>
    lift_dl(expand_grid)()
}

#' @rdname doc-all
#' @export
#'
px_dl <- function(
  path,
  var = px_var(path, api),
  simplify_colnames = FALSE,
  na_omit = FALSE,
  api = "https://statfin.stat.fi/PXWeb/api/v1/fi/"
) {
  url <- str_c(api, path)

  # get variable labels
  res_labs <- px_nav(path, api)
  if (!("variables" %in% names(res_labs))) {
    stop("The page does not appear to contain a dataset.")
  }

  # this will only apply if var is non-null
  colname_map <- set_names(res_labs$variables$code, res_labs$variables$text)
  var_maps <- map2(
    res_labs$variables$values,
    res_labs$variables$valueTexts,
    ~ set_names(.x, .y)
  ) |>
    set_names(names(colname_map))

  fix_cn <- function(x) set_names(x, colname_map[colnames(x)])
  # map from values to valueTexts
  body <- map2(var, var_maps, ~ .y[.x]) |>
    bind_cols() |>
    # map from text to code
    fix_cn() |>
    construct_body()

  # get the actual data as JSON
  res <- POST(url, body = body)

  handle_http_errors(res)

  res$content <- remove_bom(res$content)
  res_json <- content(res, "text", "application/json", "UTF-8") |>
    fromJSON(simplifyVector = FALSE)

  dims <- res_json$dimension |>
    map(~ unname(unlist(pluck(.x, "category", "label")))) |>
    reduce(expand_grid, .name_repair = "minimal") |>
    set_names(res_json$id)
  values <- res_json$value |>
    map(~ if (is.null(.x)) NA else .x) |>
    unlist()

  res_df <- bind_cols(dims, tibble(value = values))

  if (simplify_colnames) {
    res_df <- rename_with(res_df, str_clean)
  }

  if (na_omit) drop_na(res_df) else res_df
}
