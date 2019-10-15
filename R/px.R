#' Download data from the PX-Web API of Statistics Finland
#'
#' \code{px_nav} is used to navigate the API: it returns the possible
#'  subdirectories as a tibble with dbid corresponding to the suffix in the url
#'  that points to the given subdirectory.
#'
#'  \code{px_var} returns the values of the dimensions in the given table and
#'  \code{px_dl} returns the actual data corresponding to variables specified
#'  as the \code{var} argument. The format of the \code{var} argument is the
#'  same as in the tibble returned by \code{px_var}, so the values can be
#'  selected by filtering that tibble. See the examples for mode information.
#'
#' @name doc-all
#' @param path suffix to the path after \code{api}.
#'  For \code{px_download} and \code{px_var} this must point to an actual
#'  dataset (ie. an url ending in .px), whereas for \code{px_nav} this should
#'  correspond to a partial path to a dataset (ie. an url that points to a
#'  directory). Defaults to \code{""}.
#' @param var The values of the dimensions to be queried as a tibble. Can be
#'  obtained by filtering from the result of \code{px_var}. If \code{NULL}, the
#'  defaults are selected.
#' @param simplify_colnames If \code{TRUE}, the colnames are transformed to
#'  ascii, lowercase and spaces are replaced with underscores.
#' @param na_omit If \code{TRUE}, all missing values are omitted.
#'  Defaults to \code{FALSE}.
#' @param api The url to the API. Defaults to
#'  \code{"https://pxnet2.stat.fi/PXWeb/api/v1/fi/"}, the Finnish PX-Web API of
#'  Statistics Finland.
#'
#' @return \code{px_dl} and \code{px_var} always return a tibble.
#'  \code{px_nav} returns a tibble when request is sent to a non-table URL
#'  (ie. an url corresponds to a directory rather than a particular data set).
#'
#' @examples
#' \donttest{
#' # Navigate through the API
#' px_nav()
#' # check what data is available under StatFin
#' px_nav("StatFin/")
#' # navigate through the directories,
#' # check what variables are available for population statistics
#' var <- px_var("StatFin/vrm/synt/statfin_synt_pxt_011.px",
#'               "https://pxnet2.stat.fi/PXWeb/api/v1/en/")
#' # select years after 1900
#' var_1900 <- dplyr::filter(var, Year >= 1900)
#' # Download the data, starting from year 1900,
#' # omitting the var-argument would download the data for all the years.
#' data <- px_dl("StatFin/vrm/synt/statfin_synt_pxt_011.px",
#'               var = var_1900, simplify_colnames = TRUE,
#'               api = "https://pxnet2.stat.fi/PXWeb/api/v1/en/")
#' }
#'

#' @rdname doc-all
#' @export
#'
px_nav <- function(path = "", api = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/") {

  url <- str_c(api, path)
  res <- GET(url) %T>% handle_req_errors()
  res_json <- content(res, "text", "application/json", "UTF-8") %>% fromJSON()

  if (is.data.frame(res_json)) {
    as_tibble(res_json, .name_repair = "unique")
  } else {
    modify_if(res_json, is.data.frame, as_tibble, .name_repair = "unique")
  }
}

#' @rdname doc-all
#' @export
#'
px_var <- function(path, api = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/") {

  res <- px_nav(path, api)

  if (!("variables" %in% names(res)))
    stop("The page does not appear to contain a dataset.")

  map2(res$variables$text, res$variables$valueTexts, ~tibble(!!.x := .y)) %>%
    reduce(crossing)
}

#' @rdname doc-all
#' @export
#'
px_dl <- function(path, var = NULL, simplify_colnames = FALSE, na_omit = FALSE,
                  api = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/") {

  url <- str_c(api, path)

  # get variable labels
  res_labs <- px_nav(path, api)
  if (!("variables" %in% names(res_labs)))
    stop("The page does not appear to contain a dataset.")

  # this will only apply if var is non-null
  colname_map <- set_names(res_labs$variables$code, res_labs$variables$text)
  var_maps <- map2(res_labs$variables$values, res_labs$variables$valueTexts,
      ~set_names(.x, .y)) %>%
    set_names(names(colname_map))

  # map from values to valueTexts
  body <- map2_df(var, var_maps, ~.y[.x]) %>%
    # map from text to code
    (function(x) set_names(x, colname_map[colnames(x)])) %>%
    construct_body()

  # get the actual data as JSON
  res <- POST(url, body = body)
  res$content <- remove_bom(res$content)
  res_json <- content(res, "text", "application/json", "UTF-8") %>%
    fromJSON(simplifyVector = FALSE)

  str_proc <- if (simplify_colnames) str_clean else identity

  # get colnames of all dimensions of type d and t
  col_names <- map_df(res_json$columns, ~.x[c("type", "text")]) %>%
    filter(.data$type %in% c("d", "t")) %>%
    pull("text") %T>%
    check_colname_comp(res_json)

  # map from code to text
  var_maps_inv <- map(var_maps[col_names], ~set_names(names(.x), .x))

  # get the dimensions of the data
  res_dims <- map_df(res_json$data, ~t(.x$key) %>% as_tibble(.name_repair = "unique")) %>%
    mutate_all(unlist) %>%
    map2_df(var_maps_inv, ~.y[.x]) %>%
    set_names(str_proc(col_names))

  # get the actual values of the data
  res_values <- map(res_json$data, "values") %>%
    unlist() %>%
    parse_number(na = c(".", ".."))

  res_df <- bind_cols(res_dims, tibble(value = res_values))

  if (na_omit) drop_na(res_df) else res_df
}
