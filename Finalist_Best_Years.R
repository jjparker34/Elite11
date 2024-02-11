library(tidyverse)
library(gt)

finalist_data <-  read_csv('finalist_highest_year.csv')

finalist_data <- finalist_data %>%
  distinct(Player, .keep_all = TRUE)
# full data
finalist_data %>%
  gt(rowname_col = 'Player') %>%
  tab_header(
    # Combine the image and text in one line of HTML
    title = html(
      paste0(
        "<table style='width: 100%;'><tr>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<img src='https://yt3.googleusercontent.com/QjzfaKc1XXbiKb2Yy-zqhuwilT3kqX3WbRrL7cMf8N-BlaFyXJszsEUmlcQ2amumom28QSXnyw=s900-c-k-c0x00ffffff-no-rj' style='height: 60px;' />",
        "</td>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<span style='font-weight: bold;'>Elite 11 Finalist's Best Year (2010-2023)</span>",
        "</td>",
        "</tr></table>"
      )
    ),
    subtitle = md('Elite 11 Finalists and their best year in college stats based on fantasy points points.')
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Campus2Canton || By: JJ Parker || @JParker1738') %>%
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

top_16 <- finalist_data %>%
  arrange(desc(`Fantasy Points`)) %>%  # Arrange the data first to ensure that the top players are at the top.
  slice_head(n = 16)  # Select the top 10 rows

top_16 %>%
  gt(rowname_col = 'Player') %>%
  tab_header(
    # Combine the image and text in one line of HTML
    title = html(
      paste0(
        "<table style='width: 100%;'><tr>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<span style='font-weight: bold;'>Elite 11 Finalist's Best Year (2010-2023)</span>",
        "</td>",
        "<td style='text-align: right; vertical-align: middle;'>",
        "<img src='https://yt3.googleusercontent.com/QjzfaKc1XXbiKb2Yy-zqhuwilT3kqX3WbRrL7cMf8N-BlaFyXJszsEUmlcQ2amumom28QSXnyw=s900-c-k-c0x00ffffff-no-rj' style='height: 60px;' />",
        "</td>",
        "</tr></table>"
      )),
    subtitle = md('Elite 11 Finalists and their best year in college stats based on ppr points')
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Campus2Canton || By: JJ Parker || @JParker1738') %>%
  tab_stubhead(label = md("**Player**")) %>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(`Points per Game`),
    palette = c('red3','yellow3','green3'),
    domain = c(0,17,40.1)
  ) %>%
  data_color(
    columns = c(`Fantasy Points`),
    palette = c('red3','yellow3','green3'),
    domain = c(0, 228.9, 500.6)
  )

