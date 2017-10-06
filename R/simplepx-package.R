#' SIMPLEPX
#'
#' An R package for accessing PX-Web APIs. The package might work with other
#' PX-Web APIs as well, but so far has only been tested with the API of
#' Statistics Finland.
#'
#' @docType package
#' @name simplepx
#'
#' @importFrom dplyr bind_cols bind_rows filter mutate mutate_all pull
#' @importFrom httr GET http_error POST
#' @importFrom jsonlite fromJSON toJSON unbox
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom purrr map map_int map_df map2 map2_df modify_if reduce set_names
#' @importFrom readr parse_number
#' @importFrom rlang .data "!!" ":="
#' @importFrom stats na.omit
#' @importFrom stringr str_c str_replace_all
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr crossing gather nest unnest
NULL
