library(httr2)
library(poorman)
library(stringr)
library(lubridate)
library(purrr)
library(tidyverse)

# getting comtrade data ---------------------------------------------------

## getting list of reference tables
response <- httr2::request('https://ec.europa.eu/eurostat/search-api/datasets/ds-059322/languages/en') |> # nolint
  httr2::req_perform()

last_modified <- response |>
  httr2::resp_body_json(simplifyVector = T) |>
  pluck("lastUpdateDate")|>
  lubridate::dmy_hms()


list_of_datasets <- response |>
  httr2::resp_body_json(simplifyVector = T) |>
  purrr::pluck("dimensions") |>
  unnest_longer("positions")|>
  unnest_wider("positions",names_repair = "unique") |> 
  select(parameter= 1, 
         description_parameter = 2, 
         code = 3, 
         description_code= 4) |> 
  mutate(last_modified = last_modified) |> 
  group_split(parameter)

parameters <- map(list_of_datasets,"parameter") |> unlist() |> unique() |> tolower()
map2(list_of_datasets,parameters,~write_rds(.x,file = paste0("inst/extdata/",.y,".rds")) )

