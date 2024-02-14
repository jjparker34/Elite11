library(tidyverse)
library(gt)
library(gtExtras)
library(gtsummary)

finalist_stats2010 <-  read_csv('finalist_highest_year.csv')
#Delete duplicate data
finalist_stats2010 <- finalist_stats2010 %>%
  distinct(Player, .keep_all = TRUE)
#Set NA stats to 0 for Freshman that didnt play:
finalist_stats2010$`Points per Game`[is.na(finalist_stats2010$`Points per Game`)] <- 0

MVP_stats2010 <- read_csv('MVP_highest_year.csv')



MVP_FP_AVG <- mean(MVP_stats2010$`Fantasy Points`)
MVP_PPG_AVG <- mean(MVP_stats2010$`Points per Game`)

finalist_FP_AVG <- mean(finalist_stats2010$`Fantasy Points`)
finalist_PPG_AVG <- mean(finalist_stats2010$`Points per Game`)


points_summary <- data.frame(
  Category = c("MVPs", "Finalists"),
  Fantasy_Points= c(MVP_FP_AVG,
                     finalist_FP_AVG
                     ),
  Points_Per_Game = c(MVP_PPG_AVG,
                      finalist_PPG_AVG)
  )

gt_points <- gt(points_summary) %>%
  tab_header(
    title = html(
      paste0(
        "<table style='width: 100%;'><tr>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<span style='font-weight: bold;'>College Fantasy Production Overview</span>",
        "</td>",
        "<td style='text-align: right; vertical-align: middle;'>",
        "<img src='https://yt3.googleusercontent.com/QjzfaKc1XXbiKb2Yy-zqhuwilT3kqX3WbRrL7cMf8N-BlaFyXJszsEUmlcQ2amumom28QSXnyw=s900-c-k-c0x00ffffff-no-rj' style='height: 60px;' />",
        "</td>",
        "</tr></table>"
      )),
    subtitle = md("**MVPs vs Finalist CFF Production(2010-2023).**")
  ) %>%
  cols_label(
    Category = "Category",
    Fantasy_Points = "Average Fantasy Points",
    Points_Per_Game = "Average PPG"
  ) %>%
  fmt_number(
    columns = c(Fantasy_Points, Points_Per_Game),
    decimals = 2
  ) %>%
  tab_options(
    stub.font.weight = "bold"
  ) %>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(Fantasy_Points),
    palette = c('red3', 'yellow3', 'green3'),
    domain = NULL
  ) %>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(Points_Per_Game),
    palette = c('red3', 'yellow3', 'green3'),
    domain = NULL
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Campus2Canton & Wikipedia || By: JJ Parker || @JParker1738')
print(gt_points)
