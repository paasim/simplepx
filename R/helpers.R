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

# remove umlauts and all special characters; set all to lowercase
str_clean <- function(s) {
  str_map <- set_names(c("_", "a", "o", ""), c(" ", "\u00e4", "\u00f6", "[^0-9a-z_]"))
  str_replace_all(tolower(s), str_map)
}

# check that the result contains as many column names as there are columns
check_colname_comp <- function(col_names, res_json) {
  lengths <- map_int(res_json$data, ~length(.x$key)) %>% unique()

  if (any(lengths != length(col_names))) stop("Unexpected data format.")
}

handle_req_errors <- function(res) {
  if (http_error(res))
    str_c("\nQuery returned with an error, see the message below:\n",
          rawToChar(res$content)) %>% stop()
}
