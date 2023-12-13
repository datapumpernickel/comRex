#' Performs the request to the Comex API
#'
#'
#' @param req a valid comtrade request built by the `cr_build_request()` function
#' @inheritParams cr_get_data
cr_perform_request <- function(req, requests_per_second, verbose = FALSE) {

  if (verbose) {
    cli::cli_inform(c("i" = "Performing request, which can take a few seconds, depending on the amount of data queried.")) # nolint
  }



  resp <- req |>
    httr2::req_throttle(rate = requests_per_second) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform()

  if (verbose) {
    cli::cli_inform(c("v" = "Got a response object from Comex Use `process = F` if there is an error after this step to find issues with the response object.")) # nolint
  }

  return(resp)
}
