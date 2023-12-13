#' Processes the response object
#'
#'
#' @param resp a valid httr2 response object created from the function `cr_perform_request()`
#'
#' @returns a data.frame object with the results
#' @inheritParams cr_get_data
cr_process_response <- function(resp, verbose = FALSE, tidy_cols) {
  result <- resp |>
    httr2::resp_body_string() |>
    rjstat::fromJSONstat()

    attributes(result)$url <- resp$url
    attributes(result)$time <- Sys.time()
    return(result)
}
