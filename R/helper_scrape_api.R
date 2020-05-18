################################################################################
# Author: Sebastian Carl, Ben Baldwin
# Purpose: Function for scraping pbp data from the new NFL API
# Code Style Guide: styler::tidyverse_style()
################################################################################

# Build a tidy version of scraped NFL API data
#
# @param game_id Specifies the game, token the token

get_pbp_api <- function(id) {

  #for testing only
  #id = '2001_21_STL_NE'
  #id = '2011_01_NO_GB'

  season = substr(id, 1, 4)

  path = 'https://github.com/guga31bb/nflfastR-data/raw/master/raw'

  raw_data <- readRDS(url(glue::glue('{path}/{season}/{id}.rds')))

  sched_info <- raw_data$sched_info
  id_week =  as.integer(stringr::str_extract(id, '(?<=\\_)[0-9]{2}(?=\\_)'))

  #game_info <- raw_data$data$viewer$gameDetail

  game_id = raw_data$data$viewer$gameDetail$id
  home_team = raw_data$data$viewer$gameDetail$homeTeam$abbreviation
  away_team = raw_data$data$viewer$gameDetail$visitorTeam$abbreviation
  weather = dplyr::if_else(
    is.null(raw_data$data$viewer$gameDetail$weather$shortDescription),
    NA_character_,
    raw_data$data$viewer$gameDetail$weather$shortDescription)
  stadium = dplyr::if_else(
    is.null(raw_data$data$viewer$gameDetail$stadium),
    NA_character_,
    raw_data$data$viewer$gameDetail$stadium)
    start_time = raw_data$data$viewer$gameDetail$startTime

  game_info <- data.frame(
    game_id,
    home_team,
    away_team,
    weather,
    stadium,
    start_time
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(game_id = as.character(game_id))

  plays <- raw_data$data$viewer$gameDetail$plays %>% dplyr::mutate(game_id = as.character(game_id))
  drives <- raw_data$data$viewer$gameDetail$drives %>%
    #these are already in the play by play
    dplyr::select(
      -possessionTeam.abbreviation,
      -possessionTeam.nickName,
      -possessionTeam.franchise.currentLogo.url
    ) %>%
    janitor::clean_names()
  colnames(drives) <- paste0('drive_', colnames(drives))

  stats <- tidyr::unnest(plays %>% dplyr::select(-yards), cols = c(playStats)) %>%
    dplyr::mutate(
      yards = as.integer(yards),
      statId = as.numeric(statId),
      team.abbreviation = as.character(team.abbreviation)
    ) %>%
    dplyr::rename(
      player.esbId = gsisPlayer.id,
      player.displayName = playerName,
      teamAbbr = team.abbreviation
    ) %>%
    dplyr::select(
      playId,
      statId,
      yards,
      teamAbbr,
      player.displayName,
      player.esbId
    )

  # if I don't put this here it breaks
  suppressWarnings(
    pbp_stats <-
      purrr::map_df(unique(stats$playId), function(x) {
        sum_play_stats(x, stats = stats)
      }) %>%
      dplyr::mutate(play_id = as.integer(play_id)) %>%
      dplyr::select(-penalty)
  )

  combined <- game_info %>%
    dplyr::left_join(sched_info, by = c('game_id')) %>%
    dplyr::left_join(plays %>% dplyr::select(-playStats), by = c('game_id')) %>%
    dplyr::left_join(drives, by = c('driveSequenceNumber' = 'drive_order_sequence')) %>%
    dplyr::left_join(pbp_stats, by = c('playId' = "play_id")) %>%
    dplyr::mutate_if(is.logical, as.numeric) %>%
    dplyr::mutate_if(is.integer, as.numeric) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    janitor::clean_names() %>%
    dplyr::rename(
      time = clock_time,
      play_type_nfl = play_type,
      posteam = possession_team_abbreviation,
      yardline = yard_line,
      penalty = penalty_on_play,
      sp = scoring_play,
      drive = drive_sequence_number,
      ydsnet = drive_yards,
      nfl_api_id = game_id
    ) %>%
    dplyr::mutate(
      posteam_id = posteam,
      # have to do all this nonsense to make goal_to_go and yardline_side for compatibility with later functions
      yardline_side = purrr::map_chr(stringr::str_split(yardline, " "),
                                     function(x) x[1]),
      yardline_number = as.numeric(purrr::map_chr(stringr::str_split(yardline, " "),
                                                  function(x) x[2])),
      quarter_end = dplyr::if_else(stringr::str_detect(play_description, 'END QUARTER'), 1, 0),
      game_year = as.integer(game_year),
      game_month = as.integer(game_month),
      game_id = as.character(glue::glue('{season}_{formatC(id_week, width=2, flag=\"0\")}_{away_team}_{home_team}'))
      )

  combined <-
    combined %>%
    dplyr::select(
      tidyselect::one_of(
        c(pbp_stat_columns, api_cols, save_cols)
      )
    )


  }


#otherwise scraping a lot of seasons breaks
save_cols <- c(
  "game_id", "nfl_api_id", "home_team", "away_team",
  "home", "away", "game_time",
  "gsis_id", "week_id",
  "week_season", "week_season_type",
  "week_week", "week_name", "week_week_order",
  "home_team_id", "visitor_team_id",
  "home_team_type", "visitor_team_type",
  "venue_type", "venue_id",
  "season", "game_date", "game_month",
  "game_year", "time", "down", "drive_net_yards",
  "drive", "first_down", "goal_to_go", "order_sequence",
  "play_description", "play_review_status",
  "play_type_nfl", "quarter", "sp",
  "scoring_play_type", "special_teams_play",
  "time_of_day",
  "yardline", "yards",
  "yards_to_go", "latest_play",
  "posteam", "possession_team_nick_name",
  "possession_team_franchise_current_logo_url", "scoring_team_id",
  "scoring_team_abbreviation", "scoring_team_nick_name",
  "ydsnet", "drive_yards_penalized",
  "posteam_id", "yardline_side",
  "yardline_number", "quarter_end"
)






