## Liam Jennings
## Long At-Bats


# Libraries and Functions -------------------------------------------------

## libraries
library(tidyverse)
library(gt)
library(gtExtras)


## theme set
theme_set(theme_bw())

## create theme
mlb_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # adjust x axis title
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust x axis text
    axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}


# Tables ------------------------------------------------------------------

## initial table
statcast_pitch_num |>
  # select
  select(
    pitch_number, 
    n_pitches,
    xBA:swing_and_miss_pct
  ) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  # cols_label(
  #   ride_name = "Ride Name",
  #   mean_posted_wait = "Avg Posted Wait Time",
  #   sd_posted_wait = "SD of Posted Wait Time",
  #   mean_actual_wait = "Avg Actual Wait Time",
  #   sd_actual_wait = "SD of Actual Wait Time",
  #   difference = "Difference of Posted and Actual Wait Time"
  # ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      xBA,
      xwOBA
    ),
    decimals = 3
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      launch_speed:swing_and_miss_pct
    ),
    decimals = 2
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      xwOBA
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      wOBA
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      launch_speed
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      zone_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      zone_swing_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      chase_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      chase_contact_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      swing_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      whiff_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"),
      domain = NULL
    )
  ) |>
  # title and subtitle
  tab_header(
    title = md("**How do Batters Perform as the At-Bat Gets Longer?**"),
    subtitle = md("*Data: Baseball Savant*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Gold = 'Good'<br>
                  Blue = 'Bad'*")
  ) |> 
  # theme
  gtExtras::gt_theme_espn()



# At-Bat Lengths ----------------------------------------------------------

## bin width
bw <-  2 * IQR(statcast_filter |> filter(PA == 1) |> pull(pitch_number)) / length(statcast_filter |> filter(PA == 1)) ^ (1/3)

## histogram
statcast_filter |> 
  # filter for end of PA
  filter(PA == 1) |> 
  # select only pitches thrown in plate appearance
  select(pitch_number) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = pitch_number
    )
  ) +
  # histogram
  geom_histogram(
    # bar outline color
    color = "grey50",
    # fill color
    fill = "dodgerblue4",
    # freedman-diaconis rule
    binwidth = bw, 
    # set boundary to 0
    boundary = 0
  ) +
  # turn off scientific notation
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  # labels
  labs(
    x = "Pitch Number",
    y = "Frequency",
    title = "Number of Pitches in a PA"
  ) +
  mlb_theme()


## setting up density curve
### lambda value
lambda <- 1 / mean(statcast_filter |> filter(PA == 1) |> pull(pitch_number))

### max value
max_x <- max(statcast_filter$pitch_number, na.rm = TRUE)

### x values
x_vals <- 0:max_x

### dataframe
pois_pitch <- data.frame(
  x = x_vals,
  y = dpois(x_vals, lambda = 1 / lambda)
)


## histogram with density
statcast_filter |> 
  # filter for end of PA
  filter(PA == 1) |> 
  # select only pitches thrown in plate appearance
  select(pitch_number) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = pitch_number
    )
  ) +
  # histogram
  geom_histogram(
    # density
    aes(
      y = after_stat(density)
    ), 
    # bar outline color
    color = "grey50",
    # fill color
    fill = "dodgerblue4",
    # freedman-diaconis rule
    binwidth = 1, 
    # set boundary to 0
    boundary = 0
  ) +
  # density curve
  geom_line(
    # data
    data = pois_pitch,
    # aesthetics
    aes(
      x = x,
      y = y
    ),
    # color
    color = "firebrick" 
  ) +
  # turn off scientific notation
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  # labels
  labs(
    x = "Pitch Number",
    y = "Frequency",
    title = "Number of Pitches in a PA"
  ) +
  mlb_theme()



# Classify a Long At-Bat --------------------------------------------------

# data
at_bat_length <- {
  statcast_filter |> 
    
    # filter
    filter(PA == 1) |> 
    
    # mutate
    mutate(
      # at-bat length
      AB_length = case_when(
        pitch_number %in% c(1:3) ~ "Short",
        pitch_number %in% c(4:6) ~ "Average",
        pitch_number %in% c(7:16) ~ "Long",
      ),
      
      # turn into factor
      AB_length = factor(
        # column
        AB_length,
        # levels
        levels = c(
          "Short", "Average", "Long"
        )
      )
    ) |> 
      
    # group by batter and season
    group_by(
      AB_length
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
  
  
## table
at_bat_length |> 
  # select
  select(
    AB_length, 
    xBA:swing_and_miss_pct
  ) |> 
  # gt table
  gt(
    # rowname column
    rowname_col = "AB_length"
  ) |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  # cols_label(
  #   ride_name = "Ride Name",
  #   mean_posted_wait = "Avg Posted Wait Time",
  #   sd_posted_wait = "SD of Posted Wait Time",
  #   mean_actual_wait = "Avg Actual Wait Time",
  #   sd_actual_wait = "SD of Actual Wait Time",
  #   difference = "Difference of Posted and Actual Wait Time"
  # ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      xBA,
      xwOBA
    ),
    decimals = 3
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      launch_speed:swing_and_miss_pct
    ),
    decimals = 2
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      xwOBA
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      wOBA
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      launch_speed
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      zone_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      zone_swing_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      chase_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      chase_contact_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      swing_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"),
      domain = NULL
    )
  ) |>
  # add color
  data_color(
    # columns
    columns = c(
      whiff_pct
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"),
      domain = NULL
    )
  ) |>
  # title and subtitle
  tab_header(
    title = md("**How do Batters Perform as the At-Bat Gets Longer?**"),
    subtitle = md("*Data: Baseball Savant*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Gold = 'Good'<br>
                  Blue = 'Bad'*")
  ) |> 
  # theme
  gtExtras::gt_theme_espn()

