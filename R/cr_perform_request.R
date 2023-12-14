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
    httr2::req_error(body = error_body) |>
    httr2::req_throttle(rate = requests_per_second) |>
    httr2::req_retry(max_tries = 20, backoff = backoff, is_transient = is_transient) |>
    httr2::req_perform()

  if (verbose) {
    cli::cli_inform(c("v" = "Got a response object from Comex Use `process = F` if there is an error after this step to find issues with the response object.")) # nolint
  }

  return(resp)
}

error_body <- function(resp) {
  if(resp$status_code==413){

    if(httr2::resp_status(resp) == 413 &&
       !grepl("ASYNCHRONOUS",httr2::resp_body_json(resp)$error[[1]]$label)){
      cli::cli_inform(c("!" =httr2::resp_body_json(resp)$error[[1]]$label,
                        "i" = "Try adding more parameters to reduce the size of your request"))
    }

  }

}

is_transient <- function(resp) {
  httr2::resp_status(resp) == 413 &&
    grepl("ASYNCHRONOUS",httr2::resp_body_json(resp)$error[[1]]$label)
}

backoff <- function(attempts){
  attempts*30
}

