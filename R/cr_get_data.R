#' Get trade data from the Comex API
#'
#' This function will get Comex data from the new API.
#'
#' @details
#'
#' @export
#' @returns A data.frame with trade data or, if `process = F`, a httr2 response object.

cr_get_data <- function(freq,
                        reporter,
                        partner,
                        stat_procedure,
                        product,
                        time,
                        indicators,
                        flow,
                        process = TRUE,
                        tidy_cols = TRUE,
                        verbose = FALSE,
                        update = T,
                        requests_per_second = 10 / 60) {
  ## compile codes
  params <- cr_check_params(
    freq = freq,
    reporter = reporter,
    partner = partner,
    stat_procedure = stat_procedure,
    product = product,
    time = time,
    indicators = indicators,
    flow = flow,
    update = update,
    verbose = verbose
    )

  req <-
    cr_build_request(params, verbose = verbose)

  resp <- cr_perform_request(req,
                             requests_per_second = requests_per_second,
                             verbose = verbose)

  if (process) {
    result <- cr_process_response(resp, verbose = verbose,
                                  tidy_cols = tidy_cols)
    return(result)
  } else{
    return(resp)
  }
}
