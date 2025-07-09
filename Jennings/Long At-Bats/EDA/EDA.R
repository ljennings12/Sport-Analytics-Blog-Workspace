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
