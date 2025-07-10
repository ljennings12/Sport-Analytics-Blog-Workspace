## Liam Jennings
## Long At-Bats


# Libraries and Functions -------------------------------------------------

library(tidyverse)


# Read in Data --------------------------------------------------------------------

## 2021-24 statcast pitch-by-pitch data
statcast <- read_csv("statcast.csv") |> 
  # remove index column
  select(-1)

## wOBA Constants
woba_constants <- read_csv("wOBA_constants.csv")

## glimpse
glimpse(statcast)



# Clean Data --------------------------------------------------------------

## calculate metrics for each pitch
statcast_filter <- {
  statcast |> 
    # mutate
    mutate(
      # change game_year to Season
      season = factor(game_year, levels = c(2021, 2022, 2023, 2024)),
      
      # Create a swing indicator
      swing = ifelse(description %in%
                       c("bunt_foul_tip",
                         "foul", "foul_bunt",
                         "foul_pitchout",
                         "foul_tip", "hit_into_play",
                         "missed_bunt", "swinging_strike",
                         "swinging_strike_blocked"), 
                     1, 0),
      
      
      # Create an indicator for a missed swing attempt
      miss = ifelse(description %in%
                      c("missed_bunt", "swinging_strike",
                        "swinging_strike_blocked", "foul_tip",
                        "bunt_foul_tip"), 
                    1, 0),
      
      # single
      X1B = if_else(
        # condition
        events == "single",
        1,
        0
      ),
      
      # double
      X2B = if_else(
        # condition
        events == "double",
        1,
        0
      ),
      
      # triple
      X3B = if_else(
        # condition
        events == "triple",
        1,
        0
      ),
      
      # home run
      HR = if_else(
        # condition
        events == "home_run",
        1,
        0
      ),
      
      # unintentional walk
      uBB = if_else(
        # condition
        events == "walk",
        1,
        0
      ),
      
      # hit by pitch
      HBP = if_else(
        # condition
        events == "hit_by_pitch",
        1,
        0
      ),
      
      # sacrifice flies
      SF = if_else(
        # condition
        events %in% c("sac_fly", "sac_fly_double_play"),
        1,
        0
      ),
      
      # at-bats
      AB = if_else(
        # condition
        events %in% c(
          "double",
          "double_play",
          "field_error",
          "field_out",
          "fielders_choice",
          "fielders_choice_out",
          "force_out",
          "grounded_into_double_play",
          "home_run",
          "single",
          "strikeout",
          "strikeout_double_play",
          "triple",
          "triple_play"
        ),
        1,
        0
      ),
      
      # plate appearance
      PA = if_else(
        # condition
        events != "truncated_pa" | !(is.na(events)),
        1,
        0
      )
      
      # not worrying about platoon advantages / handedness standardization
    ) |> 
      
      # left join wOBA constants
      left_join(
        woba_constants,
        # by
        by = c("game_year" = "Season")
      ) |> 
      
      # calculate wOBA for each season using wOBA constans
      mutate(
        wOBA = (
          # unintentional walks
          (wBB * uBB) + 
            # hit by pitches
            (wHBP * HBP) + 
            # singles
            (w1B * X1B) +   
            # doubles
            (w2B * X2B) +
            # triples
            (w3B * X3B) + 
            # home runs
            (wHR * HR)
        ) / 
          # denominator
          (AB + uBB + SF + HBP)
      )
}


## calculate for each pitch number in at-bat
statcast_pitch_num <- {
  statcast_filter |> 
    
    # group by batter and season
    group_by(
      pitch_number
    ) |> 
    
    # hitter summary stats
    summarize(
      # counting stats
      n_pitches = n(),
      n_swings = sum(swing, na.rm = TRUE),
      n_miss = sum(miss, na.rm = TRUE),
      swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
      n_in_zone_swing = sum(swing == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing = sum(swing == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_contact = sum(swing == 1 & miss == 0 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_contact = sum(swing == 1 & miss == 0 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_swing_and_miss = sum(swing == 1 & miss == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing_and_miss = sum(swing == 1 & miss == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_pitch_in_zone = sum(zone %in% c(1:9), na.rm = TRUE),
      n_pitch_out_zone = sum(!(zone %in% c(1:9)), na.rm = TRUE),
      
      # number of plate appearances
      PA = sum(PA, na.rm = TRUE),
      
      # metrics
      
      ## expected batting average
      xBA = mean(estimated_ba_using_speedangle, na.rm = TRUE),
      
      ## xwOBA
      xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
      
      # wOBA
      wOBA = mean(wOBA, na.rm = TRUE),
      
      ## launch speed
      launch_speed = mean(
        launch_speed[!(is.na(hit_location))], 
        na.rm = TRUE
      ),
      
      ## launch angle
      launch_angle = mean(
        launch_angle[!(is.na(hit_location))], 
        na.rm = TRUE
      )
      
    ) |> 
    
    # calculate common baseball stats
    mutate(
      # Zone %
      zone_pct = round(n_pitch_in_zone / n_pitches, 6) * 100,
      
      # Zone Swing %
      zone_swing_pct = round(n_in_zone_swing / n_pitch_in_zone, 6) * 100,
      
      # Zone Contact %
      zone_contact_pct = round(n_in_zone_contact / n_in_zone_swing, 6) * 100,
      
      # Zone Swing and Miss %
      zone_swing_and_miss_pct = round(n_in_zone_swing_and_miss / n_in_zone_swing, 6) * 100,
      
      # Chase %
      chase_pct = round(n_out_zone_swing / n_pitch_out_zone, 6) * 100,
      
      # Chase Contact %
      chase_contact_pct = round(n_out_zone_contact / n_out_zone_swing, 6) * 100,
      
      # Chase swing and miss pct
      chase_swing_and_miss_pct = round(n_out_zone_swing_and_miss / n_out_zone_swing, 6) * 100,
      
      # Swing %
      swing_pct = round(n_swings / n_pitches, 6) * 100,
      
      # Whiff %
      whiff_pct = round(n_miss / n_swings, 6) * 100,
      
      # Swing and miss Rate
      swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100
    ) |> 
    
    # ungroup
    ungroup()
}


