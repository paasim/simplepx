# construct the body for the post request
construct_body <- function(var_code) {

  if (nrow(var_code) == 0) {
    query <- tibble()
  } else {
    # transform the tibble back to the original format
    query <- gather(var_code, "code", "values") %>%
      unique() %>%
      nest(-"code", .key = "selection") %>%
      mutate(selection = map(.data$selection,
                             # the actual values + {filter = item} for the JSON
                             ~as.list(.x) %>% c(list(filter = unbox("item")))))
  }

  list(query = query, response = unbox(tibble(format = "json"))) %>%
    toJSON()
}

# remove the byte-order-mark that causes problems with jsonlite
remove_bom <- function(r)  if (all(r[1:3]==c("ef","bb","bf"))) r[-(1:3)] else r

# remove accents and switch spaces to underscores
str_clean <- function(s) {
  stri_trans_general(s, "latin-ascii") %>%
    str_squish() %>%
    str_replace_all(" ", "_") %>%
    tolower()
}

# check that the result contains as many column names as there are columns
check_colname_comp <- function(col_names, res_json) {
  lengths <- map_int(res_json$data, ~length(.x$key)) %>% unique()

  if (any(lengths != length(col_names))) stop("Unexpected data format.")
}

handle_req_errors <- function(res) {
  if (http_error(res))
    str_c("\nQuery returned with an error, see the message below:\n",
          content(res, "text", "application/json", "UTF-8")) %>% stop()

  if (!str_detect(pluck(res, "headers", "content-type"), "application/json"))
    stop("A non-JSON result obtained. Perhaps an invalid url?")
}

handle_http_errors <- function(res) {
  status <- http_status(res)
  if (status$category != "Success")
    stop(str_c("Request failed with '", status$message,
               "', perhaps trying to download over a file",
               "that is bigger than the API limit (1M rows)?"))
}


.onAttach <- function(...) {
  ver <- utils::packageVersion("simplepx")
  packageStartupMessage("This is simplepx version ", ver)
}