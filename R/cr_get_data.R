#' Get trade data from the Comex API
#'
#' This function will get Comex data from the new API.
#'  @param ds_id This will let you define which dataset you want to query.
#'  Most likely you want: 045409, as it is the standard trade dataset. Check
#'  `https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/dataflow/ESTAT/all` for all available datasets.
#'
#'
#' @details
#'
#' @export
#' @returns A data.frame with trade data or, if `process = F`, a httr2 response object.

cr_get_data <- function(freq,
                        reporter,
                        partner,
                        product,
                        time,
                        indicators,
                        flow,
                        process = TRUE,
                        tidy_cols = TRUE,
                        verbose = FALSE,
                        update = T,
                        requests_per_second = 10 / 60,
                        ds_id = "045409" ) {

  ## compile codes
  params <- cr_check_params(
    freq = freq,
    reporter = reporter,
    partner = partner,
    product = product,
    time = time,
    indicators = indicators,
    flow = flow,
    update = update,
    verbose = verbose
    )

  req <-
    cr_build_request(params,ds_id, verbose = verbose)

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
