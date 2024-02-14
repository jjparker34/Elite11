library(tidyverse)
library(shiny)
library(ggplot2)
library(gt)

Sys.setenv(CFBD_API_KEY = "S2JgCsvJ5RPuw2dDwPquZ1xTKefAk5UT/H0qeK9jZAAH46WMsGbKtfRJ4odTswoX")
Elite11Players <- read_csv("Elite11Players.csv")
#split elite 11 players into First and Last name
name_parts <- strsplit(Elite11Players$Name, " ")
Elite11Players$First_Name <- sapply(name_parts, function(x) ifelse(length(x) >= 2, x[1], x))
Elite11Players$Last_Name <- sapply(name_parts, function(x) ifelse(length(x) >= 2, paste(x[-1], collapse = " "), ""))


#Elite 11 MVPs
MVPs <- Elite11Players %>%
  filter(stringr::str_detect(Awards, "Elite"))

MVPs_after_2010 <- MVPs %>%
  filter(Year >= 2010)



MVP_stats_avg <- read_csv("MVP_stats_avg.csv")
MVP_stats_yearly <- read_csv("MVP_stats.csv")

#Add Asiantii Woulard 
new_player <- data.frame(
  Player = "Asiantii Woulard",
  Team = "UCLA",
  Season = 2014, 
  Class = 2013,
  `Year out of HS` = 1, 
  `Year Played` = 1, 
  `Games Played` = 1, 
  `Fantasy Points` = 0, 
  `Points per Game` = 0, 
  `Draft Pick Overall` = NA,
  check.names = FALSE
)



# Find MVP's best PPR year

highest_ppr_year <- MVP_stats_yearly %>%
  add_row(new_player) %>%
  distinct(Player, .keep_all = TRUE) %>%  # Remove any duplicate players
  group_by(Player) %>%
  slice_max(order_by = `Fantasy Points`, n = 1) %>%  # Get the top row per group
  select(Player, Team, Class, Season, `Year out of HS`, `Games Played`, `Fantasy Points`, `Points per Game`, `Draft Pick Overall`) %>%
  arrange(desc(`Fantasy Points`)) %>%
  ungroup()


highest_ppr_year %>%
  filter(unique(Player))

write.csv(highest_ppr_year, file = 'MVP_highest_year.csva')

highest_ppr_year %>%
  gt(rowname_col = 'Player') %>%
  tab_header(
    # Combine the image and text in one line of HTML
    title = html(
      paste0(
        "<table style='width: 100%;'><tr>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<span style='font-weight: bold;'>Elite 11 MVPs Best Year (2010-2023)</span>",
        "</td>",
        "<td style='text-align: right; vertical-align: middle;'>",
        "<img src='https://yt3.googleusercontent.com/QjzfaKc1XXbiKb2Yy-zqhuwilT3kqX3WbRrL7cMf8N-BlaFyXJszsEUmlcQ2amumom28QSXnyw=s900-c-k-c0x00ffffff-no-rj' style='height: 60px;' />",
        "</td>",
        "</tr></table>"
      )
    ),
    subtitle = md('Elite 11 MVP and their best year in college stats based on fantasy points')
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Campus2Canton || By: JJ Parker || @JParker1738') %>%
  tab_footnote(
    footnote = 'Projected 1st overall pick.',
    locations = cells_stub(rows = which(highest_ppr_year$Player == 'Caleb Williams'))
  ) %>%
  tab_footnote(
    footnote = 'Projected 4th round pick.',
    locations = cells_stub(rows = which(highest_ppr_year$Player == 'Spencer Rattler'))
  ) %>%
  tab_footnote(
    footnote = "Jackson Arnold's True Freshman Season.",
    locations = cells_stub(rows = which(highest_ppr_year$Player == 'Jackson Arnold'))
  ) %>%
  tab_stubhead(label = md("**Player**")) %>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(`Points per Game`),
    palette = c('red3','yellow3','green3'),
    domain = NULL
  ) %>%
  data_color(
    columns = c(`Fantasy Points`),
    palette = c('red3','yellow3','green3'),
    domain = NULL
  )
