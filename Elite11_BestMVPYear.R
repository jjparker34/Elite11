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
  `PPR Points` = 0, 
  `PPR Points per Game` = 0, 
  `Draft Pick Overall` = NA,
  check.names = FALSE
)



# Find MVP's best PPR year

highest_ppr_year <- MVP_stats_yearly %>%
  add_row(new_player) %>%
  group_by(Player) %>%
  top_n(1, `PPR Points`) %>%
  select(Player,Team,Class,Season,`Year out of HS`,`Games Played`,`PPR Points`,`PPR Points per Game`,`Draft Pick Overall`)%>%
  arrange(desc(`PPR Points`)) %>%
  ungroup()

highest_ppr_year %>%
  gt(rowname_col = 'Player') %>%
  tab_header(
    # Combine the image and text in one line of HTML
    title = html(
      paste0(
        "<img src='https://highschoolfootballamerica.com/wp-content/uploads/2020/06/elite-11-scaled.jpg' style='height: 60px;' />",
        "<br>",
        "<span style='font-weight: bold;'>Elite 11 MVPs Best Year (2010-2023)</span>"
      )
    ),
    subtitle = md('Elite 11 MVP and their best year in college stats based on ppr points')
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
    columns = c(`PPR Points`),
    method = "quantile",
    palette = "viridis",
    quantiles = 5
  ) %>%
  data_color(
    columns = c(`PPR Points per Game`),
    method = "quantile",
    palette = "viridis",
    quantiles = 5
  )
