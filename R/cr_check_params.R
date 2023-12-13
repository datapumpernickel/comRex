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
    cli::cli_inform(c("v" = "Checked validity of produt"))
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
  
  time <- check_date(time, frequency)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of time."))
  }  
  
  stat_procedure <- check_date(stat_procedure)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of stat_procedure"))
  }  
  
  indicators <- check_date(indicators)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of indicators"))
  }
  
  params <- list(
    url_params = list(freq = freq,
                      reporter = reporter,
                      partner = partner,
                      stat_procedure = stat_procedure,
                      product = product,
                      time = time,
                      indicators = indicators,
                      flow = flow)
  )
  
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
check_freq <- function(type, frequency) {
  # only annual data for services endpoint
  if(type == 'S'){
    rlang::arg_match(frequency, values = c("A"))
  } else {
    rlang::arg_match(frequency, values = c("A", "M"))
  }
  return(frequency)
}

#' Check validity of classification parameter.
#'
#' Trade (IMTS) classifications: HS, SITC, BEC or EBOPS.
#' Currently, we only support the HS classification.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character string specifying the selected classification code.
#'
#' @examplesIf interactive()
#' comtradr:::check_clCode("HS")
#' # returns "HS"
#' comtradr:::check_clCode("ISIC")
#' # throws an error because "ISIC" is not a valid classification code
#'
#' @noRd
check_clCode <- function(type,commodity_classification) {
  cmd_list_goods <- c('HS','S1','S2','S3','S4','SS',"B4","B5")
  cmd_list_services <- c('EB02','EB10','EB10S','EB')
  if(type == 'C'){
    rlang::arg_match(commodity_classification, values = cmd_list_goods)
  } else {
    rlang::arg_match(commodity_classification, values = cmd_list_services)
  }
  return(commodity_classification)
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
check_flowCode <- function(flow_direction) {
  if(!is.null(flow_direction)){
    rlang::arg_match(
      flow_direction,
      values = c('import', 'export', 're-export', 're-import', 'all'),
      multiple = TRUE
    )
    
    if (length(flow_direction) > 1 & any(flow_direction == 'all')) {
      rlang::abort("You can only provide 'all' as a single argument.")
    }
    
    if (length(flow_direction) > 1 | !any(flow_direction == 'all')) {
      flow_direction <- stringr::str_replace_all(flow_direction,
                                                 '^import$', "M")
      flow_direction <- stringr::str_replace_all(flow_direction,
                                                 '^export$', "X")
      flow_direction <- stringr::str_replace_all(flow_direction,
                                                 '^re-import$', "RM")
      flow_direction <- stringr::str_replace_all(flow_direction,
                                                 '^re-export$', "RX")
      flow_direction <- flow_direction |> paste0(collapse = ',')
    } else if (flow_direction == 'all') {
      flow_direction <- 'M,X,RM,RX'
    }
  }
  return(flow_direction)
}

#' Check validity of commodity code parameter.
#'
#' Commodity code. We currently only support HS codes.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector specifying the commodity codes requested.
#'
#' @examplesIf interactive()
#' check_cmdCode("01") # returns "01"
#' check_cmdCode(c("01", "02")) # returns "01,02"
#' check_cmdCode("ABC") # throws an error because "ABC" is not a valid HS code
#' check_cmdCode(NULL) # throws an error because at least one HS code must be provided
#'
#' @noRd
check_cmdCode <-
  function(commodity_classification,
           commodity_code,
           update = FALSE,
           verbose = FALSE) {
    # check that commodity_code code is not null
    if (!is.null(commodity_code)) {
      commodity_code <- as.character(commodity_code)
      
      # remove any white space from cmd codes provided
      commodity_code <- stringr::str_squish(commodity_code)
      
      # get the list of valid parameters from inst/extdata
      valid_codes <-
        ct_get_ref_table(dataset_id = commodity_classification,
                         update = update,
                         verbose = verbose)$id
      
      # if one of the codes is not in the list of valid codes
      # send stop signal and list problems
      if (!all(commodity_code %in% valid_codes)) {
        rlang::abort(paste0(
          "The following services/commodity codes you provided are invalid: ",
          paste0(setdiff(commodity_code, valid_codes), collapse = ", ")
        ))
      } else {
        commodity_code <- paste0(commodity_code, collapse = ',')
      }
    }
    
    return(commodity_code)
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
#' @examplesIf interactive()
#' check_reporterCode("USA") # returns "842,841"
#' check_reporterCode(c("USA", "FRA")) # returns "251,842,841"
#' check_reporterCode("all") # returns all country codes, excluding any country groupings
#'
#' @noRd
check_reporterCode <- function(reporter, update = FALSE, verbose = FALSE) {
  iso_3 <- id <- group <- NULL
  # check that reporter code is valid
  if (!is.null(reporter)) {
    reporter <- as.character(reporter)
    
    ## check if valid reporter code length and type
    reporter <- stringr::str_squish(reporter)
    
    reporter_codes <-
      ct_get_ref_table(dataset_id = 'reporter',
                       update = update,
                       verbose = verbose)
    
    ## get multiple values or single values that are not 'all'
    if (length(reporter) > 1 | !any(reporter == 'all')) {
      if (any(reporter == 'all')) {
        rlang::abort('"all" can only be provided as a single argument.')
      }
      # if one of the reporter codes is not in the list of valid reporter codes
      # send stop signal and list problems
      if (!all(reporter %in% reporter_codes$iso_3)) {
        rlang::abort(paste0(
          "The following reporter(s) you provided are invalid: ",
          paste0(setdiff(reporter, reporter_codes$iso_3), collapse = ", ")
        ))
      }
    }
    
    # create proper ids for reporter Code
    if (length(reporter) > 1 | !any(reporter == 'all')) {
      reporter <- reporter_codes |>
        poorman::filter(iso_3 %in% reporter) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (reporter == 'all') {
      reporter <- reporter_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ',')
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
#' @examplesIf interactive()
#' check_partnerCode("CAN") # returns "124"
#' check_partnerCode(c("CAN", "MEX")) # returns "124,484"
#' check_partnerCode("all") # returns all partner codes, excluding country groupings
#'
#' @noRd
check_partnerCode <- function(partner, update = FALSE, verbose = FALSE) {
  ## evade checks in RMD Check about 'no visible binding...'
  iso_3 <- id <- group <- NULL
  
  # check that partner code is valid
  if (!is.null(partner)) {
    partner <- as.character(partner)
    
    partner_codes <- ct_get_ref_table(dataset_id = 'partner',
                                      update = update, verbose = verbose)
    
    
    if (length(partner) > 1 | !any(partner == 'all')) {
      partner <- stringr::str_squish(partner)
      if (any(partner == 'all')) {
        rlang::abort('"all" can only be provided as a single argument.')
      }
      # if one of the partnerCodes is not in the list of valid partnerCodes send stop signal and list problems
      if (!all(partner %in% partner_codes$iso_3)) {
        rlang::abort(paste(
          "The following partner you provided are invalid: ",
          setdiff(partner, partner_codes$iso_3), collapse = ", ")
        )
      }
    }
    
    # create proper ids for partner
    if (length(partner) > 1 | !any(partner == 'all')) {
      partner <- partner_codes |>
        poorman::filter(iso_3 %in% partner) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (partner == 'all') {
      partner <- partner_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    }
  }
  
  return(partner)
}


#' Check validity of partner_2 parameter.
#'
#' This function checks that the given partner_2 code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector of valid partner_2 IDs.
#'
#' @examplesIf interactive()
#' check_partner2Code("CAN") # returns "124"
#' check_partner2Code(c("CAN", "MEX")) # returns "124,484"
#' check_partner2Code("all") # returns all partner codes, excluding country groupings
#'
#' @noRd
check_partner2Code <- function(partner, update = FALSE, verbose = FALSE) {
  iso_3 <- id <- group <- NULL
  
  # check that partner code is valid
  if (!is.null(partner)) {
    partner <- as.character(partner)
    
    
    partner_codes <- ct_get_ref_table(dataset_id = 'partner',
                                      update = update, verbose = verbose)
    
    
    if (length(partner) > 1 | !any(partner == 'all')) {
      partner <- stringr::str_squish(partner)
      if (any(partner == 'all')) {
        rlang::abort('"all" can only be provided as a single argument.')
      }
      # if one of the partnerCodes is not in the list of valid partnerCodes send stop signal and list problems
      if (!all(partner %in% partner_codes$iso_3)) {
        rlang::abort(paste(
          "The following partner_2 you provided are invalid: ",
          setdiff(partner, partner_codes$iso_3), collapse = ", ")
        )
      }
    }
    
    # create proper ids for partner
    if (length(partner) > 1 | !any(partner == 'all')) {
      partner <- partner_codes |>
        poorman::filter(iso_3 %in% partner) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (partner == 'all') {
      partner <- partner_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    }
  }
  
  return(partner)
}


#' Check validity of mode of transport parameter.
#'
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector specifying the modes of transport requested.
#'
#' @noRd
check_motCode <- function(mode_of_transport, update = FALSE, verbose = FALSE) {
  # check that commodity_code code is not null
  if (!is.null(mode_of_transport)) {
    mode_of_transport <- as.character(mode_of_transport)
    
    # remove any white space from cmd codes provided
    mode_of_transport <- stringr::str_squish(mode_of_transport)
    
    # get the list of valid parameters from inst/extdata
    valid_codes <- ct_get_ref_table(dataset_id = 'mot',
                                    update = update,
                                    verbose = verbose)$id
    
    # if one of the codes is not in the list of valid codes
    # send stop signal and list problems
    if (!all(mode_of_transport %in% valid_codes)) {
      rlang::abort(paste0(
        "The following mode_of_transport codes you provided are invalid: ",
        paste0(setdiff(mode_of_transport, valid_codes), collapse = ", ")
      ))
    } else {
      mode_of_transport <- paste0(mode_of_transport, collapse = ',')
    }
  }
  
  return(mode_of_transport)
}

#' Check validity of customs parameter.
#'
#'
#' @inheritParams cr_get_data
#'
#' @returns A character vector specifying the custom codes requested.
#'
#' @noRd
check_customsCode <- function(customs_code, update = FALSE, verbose = FALSE) {
  # check that commodity_code code is not null
  if (!is.null(customs_code)) {
    customs_code <- as.character(customs_code)
    
    # remove any white space from cmd codes provided
    customs_code <- stringr::str_squish(customs_code)
    
    # get the list of valid parameters from inst/extdata
    valid_codes <- ct_get_ref_table(dataset_id = 'customs',
                                    update = update,
                                    verbose = verbose)$id
    
    # if one of the codes is not in the list of valid codes
    # send stop signal and list problems
    if (!all(customs_code %in% valid_codes)) {
      rlang::abort(paste0(
        "The following customs_code codes you provided are invalid: ",
        paste0(setdiff(customs_code, valid_codes), collapse = ", ")
      ))
    } else {
      customs_code <- paste0(customs_code, collapse = ',')
    }
  }
  
  return(customs_code)
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
#' @examplesIf interactive()
#' check_date(2010, 2011, 'A') # returns "2010,2011"
#' check_date(2010, 2011, 'A') # returns "2010"
#' check_date("2010-01", "2010-07", "M") # returns "201001,201002,201003,201004,201005,201006,201007"
#'
#' @noRd
check_date <- function(start_date, end_date, frequency) {
  
  if(is.null(start_date)|is.null(end_date)){
    rlang::abort('Please provide a start and end date for the period of interest.')
  }
  
  start_date <- as.character(start_date)
  end_date <- as.character(end_date)
  
  if (frequency == "A") {
    # Date range when freq is "annual" (date range by year)
    start_date <- convert_to_date(date_obj = start_date)
    end_date <- convert_to_date(date_obj = end_date)
    date_range <- seq.Date(start_date, end_date, by = "year") |>
      format(format = "%Y")
  } else if (frequency == "M") {
    # Date range when freq is "monthly"
    sd_year <- is_year(start_date)
    ed_year <- is_year(end_date)
    if (sd_year && ed_year) {
      # If start_date and end_date are both years ("yyyy") and are identical,
      # return the single year as the date range.
      if (identical(start_date, end_date)) {
        start_date <- convert_to_date(start_date)
        date_range <- seq.Date(start_date, by = "month",length.out = 12) |>
          format(format = "%Y%m")
      } else {
        rlang::abort("Cannot get more than a single year's worth of monthly data in a single query.") # nolint
      }
    } else if (!sd_year && !ed_year) {
      # If neither start_date nor end_date are years, get date range by month.
      start_date <- convert_to_date(start_date)
      end_date <- convert_to_date(end_date)
      date_range <- seq.Date(start_date, end_date, by = "month") |>
        format(format = "%Y%m")
    } else {
      # Between start_date and end_date, if one is a year and the other isn't,
      # throw an error.
      rlang::abort("If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.") # nolint
    }
  }
  
  # If the derived date range is longer than five elements, throw an error.
  if (length(date_range) > 12 ) {
    rlang::abort("If specifying years/months, cannot search more than twelve consecutive years/months in a single query.") # nolint
  }
  
  return(paste(date_range, collapse = ","))
}


#' Given a numeric or character date, convert to an object with class "Date".
#'
#' @returns Object of class "Date" (using base::as.Date()).
#' @noRd
convert_to_date <- function(date_obj) {
  # Convert to Date.
  if (is_year(x = date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01-01"), format = "%Y-%m-%d")
  } else if (is_year_month(x = date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01"), format = "%Y-%m-%d")
  } else {
    date_obj <- as.Date(date_obj, format = "%Y-%m-%d")
  }
  # If conversion to Date failed, throw error.
  if (is.na(date_obj)) {
    rlang::abort(
      paste("arg must be a date with one of these formats:\n",
            "int: yyyy\n",
            "char: 'yyyy'\n",
            "char: 'yyyy-mm'\n",
            "char: 'yyyy-mm-dd'"))
  }
  
  return(date_obj)
}


#' Is input a year string or not.
#'
#' @noRd
is_year <- function(x) {
  grepl("^\\d{4}$", x)
}


#' Is input a year-month string or not.
#'
#' @noRd
is_year_month <- function(x) {
  grepl("^\\d{4}-\\d{2}", x)
}