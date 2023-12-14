#' Check that input parameters are valid and in compliance with COMEX APIs.
#'
#' @inheritParams cr_get_data
cr_check_params <- function(freq,
                            reporter,
                            partner,
                            stat_procedure,
                            product,
                            time,
                            indicators,
                            flow,
                            update,
                            verbose) {


  freq <- check_freq(freq)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of freq"))
  }

  product <- check_product(product)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of product"))
  }

  flow <- check_flow(flow)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of flow"))
  }


  reporter <- check_reporter(reporter,update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of reporter."))
  }

  partner <- check_partner(partner,update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of partner."))
  }

  time <- check_date(time, update, verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of time."))
  }

  stat_procedure <- check_stat_procedure(stat_procedure, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of stat_procedure"))
  }

  indicators <- check_indicators(indicators, update, verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of indicators"))
  }

  params <- list(freq = freq,
                      reporter = reporter,
                      partner = partner,
                      stat_procedure = stat_procedure,
                      product = product,
                      time = time,
                      indicators = indicators,
                      flow = flow)

  return(params)
}


#' Check validity of freq parameter.
#'
#' Trade frequency: 'A' for annual and 'M' for monthly.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character string specifying the frequency of the data.
#'
#' @examplesIf interactive()
#' check_freq("A") # returns "A"
#' check_freq("M") # returns "M"
#' check_freq("D") # throws an error because "D" is not a valid frequency code
#'
#' @noRd
check_freq <- function(freq) {
  rlang::arg_match(freq, values = c("A", "M"))
  return(freq)
}

#' Check validity of flow direction parameter.
#'
#' Trade flow code: export, import, re-export, re-import.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector specifying the trade flow codes.
#'
#' @examplesIf interactive()
#' check_flowCode("import") # returns "M"
#' check_flowCode(c("export", "re-export")) # returns "X,RX"
#' check_flowCode("trade") # throws an error because "trade" is not a valid flow code
#' check_flowCode(NULL) # throws an error because at least one flow code must be provided
#'
#' @noRd
check_flow <- function(flow,
                       update = FALSE,
                       verbose = FALSE) {
  # check that commodity_code code is not null
  if (!is.null(flow)) {
    flow <- as.character(flow)

    # remove any white space from cmd codes provided
    flow <- stringr::str_squish(flow)

    # get the list of valid parameters from inst/extdata
    valid_codes <-
      cr_get_ref_table(dataset_id = "flow",
                       update = update,
                       verbose = verbose)$code

    # if one of the codes is not in the list of valid codes
    # send stop signal and list problems
    if (!all(flow %in% valid_codes)) {
      rlang::abort(paste0(
        "The following flow codes you provided are invalid: ",
        paste0(setdiff(flow, valid_codes), collapse = ", ")
      ))
    } else {
      flow <- flow
    }
  }

  return(flow)
}

#' Check validity of commodity code parameter.
#'#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector specifying the commodity codes requested.
#'
#' @noRd
check_product <-
  function(product,
           update = FALSE,
           verbose = FALSE) {
    # check that commodity_code code is not null
    if (!is.null(product)) {
      product <- as.character(product)

      # remove any white space from cmd codes provided
      product <- stringr::str_squish(product)

      # get the list of valid parameters from inst/extdata
      valid_codes <-
        cr_get_ref_table(dataset_id = "product",
                         update = update,
                         verbose = verbose)$code

      # if one of the codes is not in the list of valid codes
      # send stop signal and list problems
      if (!all(product %in% valid_codes)) {
        rlang::abort(paste0(
          "The following services/commodity codes you provided are invalid: ",
          paste0(setdiff(product, valid_codes), collapse = ", ")
        ))
      } else {
        product <- product
      }
    }

    return(product)
  }

#' Check validity of reporter parameter.
#'
#' This function checks that the given reporter code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid reporter IDs.
#'
#' @noRd
check_reporter <- function(reporter, update = FALSE, verbose = FALSE) {
  iso_3 <- id <- group <- NULL
  # check that reporter code is valid
  if (!is.null(reporter)) {
    reporter <- as.character(reporter)

    ## check if valid reporter code length and type
    reporter <- stringr::str_squish(reporter)

    reporter_codes <-
      cr_get_ref_table(dataset_id = 'reporter',
                       update = update,
                       verbose = verbose)

      # if one of the reporter codes is not in the list of valid reporter codes
      # send stop signal and list problems
      if (!all(reporter %in% reporter_codes$code)) {
        rlang::abort(paste0(
          "The following reporter(s) you provided are invalid: ",
          paste0(setdiff(reporter, reporter_codes$code), collapse = ", ")
        ))
      }
    }

  return(reporter)
}

#' Check validity of partner parameter.
#'
#' This function checks that the given partner code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid partner IDs.
#'
#' @noRd
check_partner <- function(partner, update = FALSE, verbose = FALSE) {
  ## evade checks in RMD Check about 'no visible binding...'
  iso_3 <- id <- group <- NULL

  # check that partner code is valid
  if (!is.null(partner)) {
    partner <- as.character(partner)

    partner_codes <- cr_get_ref_table(dataset_id = 'partner',
                                      update = update, verbose = verbose)


      if (!all(partner %in% partner_codes$code)) {
        rlang::abort(paste(
          "The following partner you provided are invalid: ",
          setdiff(partner, partner_codes$code), collapse = ", ")
        )
      }
    }

  return(partner)
}



## the get date range function was taken from https://github.com/ropensci/comtradr/blob/master/tests/testthat/test-ct_search.R # nolint

#' Check validity of date parameter.
#'
#' This function checks that the given period code is valid. If the range or
#' format is not valid, the function throws an error message indicating which
#' codes are invalid. It also converts the input to the proper format if necessary.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid reporter IDs.
#'
#' @noRd
check_date <- function(time, update, verbose) {


  # check that partner code is valid
  if (!is.null(time)) {
    time <- as.character(time)

   time_codes <- cr_get_ref_table(dataset_id = 'time',
                                      update = update,
                                  verbose = verbose)


    if (!all(time %in% time_codes$code)) {
      rlang::abort(paste(
        "The following time references you provided are invalid: ",
        setdiff(time, time_codes$code), collapse = ", ")
      )
    }
  }

  return(time)
}

#' Check validity of indicator parameter
#'
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid indiator CODEs
#'
#' @noRd
check_indicators <- function(indicators, update, verbose) {


  # check that partner code is valid
  if (!is.null(indicators)) {
    indicators <- as.character(indicators)

    indicators_codes <- cr_get_ref_table(dataset_id = 'indicators',
                                      update = update,
                                  verbose = verbose)


    if (!all(indicators %in% indicators_codes$code)) {
      rlang::abort(paste(
        "The following indicators you provided are invalid: ",
        setdiff(indicators, indicators_codes$code), collapse = ", ")
      )
    }
  }

  return(indicators)
}


#' Check validity of stat_procedure parameter
#'
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid stat_procedure CODEs
#'
#' @noRd
check_stat_procedure <- function(stat_procedure, update, verbose) {


  if (!is.null(stat_procedure)) {
    stat_procedure <- as.character(stat_procedure)

    stat_procedure_codes <- cr_get_ref_table(dataset_id = 'stat_procedure',
                                      update = update,
                                  verbose = verbose)


    if (!all(stat_procedure %in% stat_procedure_codes$code)) {
      rlang::abort(paste(
        "The following stat_procedure codes you provided are invalid: ",
        setdiff(stat_procedure, stat_procedure_codes$code), collapse = ", ")
      )
    }
  }

  return(stat_procedure)
}

