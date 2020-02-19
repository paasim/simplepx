#' SIMPLEPX
#'
#' An R package for accessing PX-Web APIs. The package might work with other
#' PX-Web APIs as well, but so far has only been tested with the API of
#' Statistics Finland.
#'
#' @docType package
#' @name simplepx
#'
#' @importFrom dplyr bind_cols bind_rows distinct filter matches mutate mutate_all
#' select
#' @importFrom httr content GET http_error http_status POST
#' @importFrom jsonlite fromJSON toJSON unbox
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom purrr lift_dl map map_int map_df map_lgl map2 map2_df map2_lgl
#' modify_if pluck reduce set_names
#' @importFrom readr parse_number
#' @importFrom rlang .data "!!" ":="
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_c str_detect str_replace_all str_squish
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom tidyr drop_na expand_grid pivot_longer nest unnest
NULL
