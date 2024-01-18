#' Build a valid request object from the checked parameters
#'
#' @returns a httr2 request object
#' @inheritParams cr_get_data
cr_build_request <- function(params,
                             verbose = FALSE,
                             ds_id) {

  params_full <- c()
  for(name in names(params)){
    values <- params[[name]]
    for(value in values){
      if(!is.null(value)){
        temp <- value
        names(temp) <- name
      }
      params_full <- c(params_full, temp)
    }
  }



  res <-
    httr2::request(glue::glue("https://ec.europa.eu/eurostat/api/comext/dissemination/statistics/1.0/data/ds-{ds_id}/")) |>
    httr2::req_url_query(!!!params_full)

  if(stringr::str_length(res$url)>4095){
    rlang::abort("Your request exceeds 4KB or 4096 characters, which is the upper limit of the Comtrade API.") # nolint
  }

  if (verbose) {
    cli::cli_inform(c("i" = paste0("URL that will be queried: ",res$url)))
  }

  return(res)
}


