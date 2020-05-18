################################################################################
# Author: Sebastian Carl and Ben Baldwin
# Purpose: Function for scraping games that have been put in github repo
# Code Style Guide: styler::tidyverse_style()
################################################################################

get_season_schedule <- function(year) {

  schedule <-
    httr::GET(
      url = glue::glue("https://api.github.com/repos/guga31bb/nflfastR-data/contents/raw/{year}")
    ) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    dplyr::select(name) %>%
    dplyr::mutate(
      name =
        stringr::str_extract(
          name, '[0-9]{4}\\_[0-9]{2}\\_[A-Z]{2,3}\\_[A-Z]{2,3}(?=.)'
        ),
      season =
        stringr::str_extract(
          name, '[0-9]{4}'
        ),
      week =
        as.integer(stringr::str_extract(name, '(?<=\\_)[0-9]{2}(?=\\_)'))
      ,
      away_team =
        stringr::str_extract(
          name, '(?<=[0-9]\\_)[A-Z]{2,3}(?=\\_)'
        ),
      home_team =
        stringr::str_extract(
          name, '(?<=[A-Z]\\_)[A-Z]{2,3}'
        ),
      season_type = dplyr::if_else(week <= 17, 'REG', 'POST')
    ) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(season, week) %>%
    dplyr::rename(game_id = name) %>%
    dplyr::distinct()

  return(schedule)


}


