
#' Get reference table from package data
#'
#' The first time, the function will read from disk, the second time from the environment. In the case of a necessary update the new data will be saved to the environment for the current session.
#' You can use this table to look at the reference tables and if necessary extract respective classification codes by hand. In general we would recommend the function `ct_commodity_lookup` for this purpose. It uses the present function in the backend.
#' @param dataset_id The dataset ID, which is either partner, reporter or a valid classification scheme.
#' @inheritParams cr_get_data
#' @export
#' @returns a tidy dataset with a reference table
#'
cr_get_ref_table <- function(dataset_id, update = FALSE, verbose = FALSE) {
  valid_list <-
    c("flow",
      "freq",
      "indicators",
      "partner",
      "product",
      "reporter",
      "stat_procedure",
      "time"
    )
  ## check dataset id for valid values
  rlang::arg_match(dataset_id, values = valid_list)

  ## attempt to return the data from the environment first
  data <- get(dataset_id, envir = cr_env)

  ## if the dataset is not yet loaded into the environment
  ## read it from disk and save to environment
  if(is.null(data)){
    data <- fs::path_package(paste0('extdata/',dataset_id,'.rds'),
                             package = 'comRex') |>
      readr::read_rds()
    assign(dataset_id,data,envir = cr_env)
  }

  if(update & any(dataset_id %in% cr_env$updated)){
    ## if update is true, but dataset_id has already been updated once
    ## only return message
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Already checked for updates for ",
                                     dataset_id,' in this session.')))
    }
    return(data)
  } else if(update){
    ## if update is true and not yet updated in this session inform user that update process is starting
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Attempting to update reference table: ",
                                     dataset_id)))
    }

    ## download new reference table from the UN
    data_new <- cr_download_ref_table(dataset_id)

    if(unique(data_new$last_modified)>unique(data$last_modified)){
      ## if the date last modified, returned in the header is newer than the old data
      if (verbose) {
        cli::cli_inform(c("i" = paste0("Updated reference tables ",
                                       dataset_id,
                                       " with new data, last modified on: ",
                                       unique(data_new$last_modified)))) # nolint
      }

      ## write to environment and overwrite old data
      assign(dataset_id,data_new,envir = cr_env)

      ## let environment variable know that dataset has been updated
      cr_env$updated <- c(cr_env$updated,dataset_id)

      return(data_new)
    } else {
      ## if last_modified is not newer, let user know that datasets are up to date.
      if (verbose) {
        cli::cli_inform(c("i" = paste0('No update necessary for table ',
                                       dataset_id,'.')))
      }

      ## save in env variable, that update has been checked in this session
      cr_env$updated <- c(cr_env$updated,dataset_id)

      return(as.data.frame(data))
    }
  } else {
    ## if no update parameter passed on, just return the data read from disk or the env
    return(as.data.frame(data))
  }
}



#' Downloading the references tables from UN Comtrade
#'
#' @noRd
cr_download_ref_table <- function(dataset_id) {

  response <- httr2::request('https://ec.europa.eu/eurostat/search-api/datasets/ds-059322/languages/en') |> # nolint
    httr2::req_perform()

  last_modified <- response |>
    httr2::resp_body_json(simplifyVector = T) |>
    purrr::pluck("lastUpdateDate")|>
    as.POSIXct( format="%d/%m/%Y %H:%M:%S")


  list_of_datasets <- response |>
    httr2::resp_body_json(simplifyVector = T) |>
    purrr::pluck("dimensions") |>
    poorman::group_split(code) |>
    purrr::map_dfr(function(.x){
      full <- .x$positions[[1]] |>
        poorman::rename(code =1, description =2) |>
        poorman::mutate(parameter = .x$code,description_parameter = .x$description)
    }) |>
    poorman::mutate(last_modified = last_modified,
                    dataset_id = tolower(parameter)) |>
    poorman::filter(dataset_id %in% dataset_id)

    return(list_of_datasets)
}
