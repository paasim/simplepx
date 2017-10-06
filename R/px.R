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
#'  directory). Defaults to \code{"/"}.
#' @param var The values of the dimensions to be queried as a tibble. Can be
#'  obtained by filtering from the result of \code{px_var}. If \code{NULL}, all
#'  the values of all the dimensions are returned.
#' @param simplify_strings If \code{TRUE}, the strings in the result (colnames,
#'  values of the dimensions) are set to all lowercase, all special characters
#'  are removed and spaces are replaced by underscores. Defaults to
#'  \code{FALSE}.
#' @param na_omit If \code{TRUE}, all missing values are omitted.
#'  Defaults to \code{FALSE}.
#' @param api The url to the API. Defaults to
#'  \code{"http://pxnet2.stat.fi/PXWeb/api/v1/en/"}, the PX-Web API of
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
#' var <- px_var("StatFin/vrm/synt/statfin_synt_pxt_011.px")
#' # select years after 1900
#' var_1900 <- dplyr::filter(var, Year >= 1900)
#' # Download the data, starting from year 1900,
#' # omitting the var-argument would download the data for all the years.
#' data <- px_dl("StatFin/vrm/synt/statfin_synt_pxt_011.px", var_1900,
#'               simplify_strings = TRUE)
#' }
#'

#' @rdname doc-all
#' @export
#'
px_nav <- function(path = "", api = "http://pxnet2.stat.fi/PXWeb/api/v1/en/") {

  url <- str_c(api, path)
  res <- GET(url) %T>% handle_req_errors()
  res_json <- rawToChar(res$content) %>% fromJSON()
  if (is.data.frame(res)) {
    as_tibble(res_json)
  } else {
    modify_if(res_json, is.data.frame, as_tibble)
  }
}

#' @rdname doc-all
#' @export
#'
px_var <- function(path = "", api = "http://pxnet2.stat.fi/PXWeb/api/v1/en/") {

  res <- px_nav(path, api)

  if (!("variables" %in% names(res)))
    stop("The page does not appear to contain a dataset.")

  map2(res$variables$text, res$variables$valueTexts, ~tibble(!!.x := .y)) %>%
    reduce(crossing)
}

#' @rdname doc-all
#' @export
#'
px_dl <- function(path, var = NULL, simplify_strings = FALSE, na_omit = FALSE,
                  api = "http://pxnet2.stat.fi/PXWeb/api/v1/en/") {

  url <- str_c(api, path)

  # get variable labels
  res_labs <- px_nav(path, api)
  if (!("variables" %in% names(res_labs)))
    stop("The page does not appear to contain a dataset.")

  var_maps <- map2(res_labs$variables$values, res_labs$variables$valueTexts,
      ~set_names(.x, .y))
  colname_maps <- set_names(res_labs$variables$code, res_labs$variables$text)

  # map from values to valueTexts
  body <- map2_df(var, var_maps, ~str_replace_all(.x, .y)) %>%
    # map from text to code
    (function(x) set_names(x, str_replace_all(colnames(x), colname_maps))) %>%
    construct_body()

  # get the actual data as JSON
  res <- POST(url, body = body) %T>% handle_req_errors()
  res_json <- remove_bom(res$content) %>%
    rawToChar() %>%
    fromJSON(simplifyVector = FALSE)

  str_proc <- if (simplify_strings) str_clean else identity

  # get colnames of all dimensions of type d and t
  col_names <- map_df(res_json$columns, ~.x[c("type", "text")]) %>%
    filter(.data$type %in% c("d", "t")) %>%
    pull("text") %>%
    str_proc() %T>%
    check_colname_comp(res_json)

  # map from code to text
  var_maps_inv <- map(var_maps, ~set_names(names(.x), .x))

  # get the dimensions of the data
  res_dims <- map_df(res_json$data, ~t(.x$key) %>% as_tibble()) %>%
    mutate_all(unlist) %>%
    map2_df(var_maps_inv, ~str_replace_all(.x, .y)) %>%
    set_names(col_names) %>%
    mutate_all(str_proc)

  # get the actual values of the data
  res_values <- map(res_json$data, "values") %>%
    unlist() %>%
    parse_number(na = c(".", ".."))

  res_df <- bind_cols(res_dims, tibble(value = res_values))

  if (na_omit) na.omit(res_df) else res_df
}