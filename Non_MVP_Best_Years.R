library(tidyverse)
library(shiny)
library(ggplot2)
library(gt)
library(cfbfastR)
library(gtsummary)
Sys.setenv(CFBD_API_KEY = "S2JgCsvJ5RPuw2dDwPquZ1xTKefAk5UT/H0qeK9jZAAH46WMsGbKtfRJ4odTswoX")

Elite11Players <- read_csv("Elite11Players.csv")
name_parts <- strsplit(Elite11Players$Name, " ")


#Split First and Last Names
Elite11Players$First_Name <- sapply(name_parts, function(x) {
  # Check if the name has more than two parts and the first part ends with a period
  if (length(x) > 2 && grepl("\\.$", x[1])) {
    # Combine the first two parts to handle names like "D. J."
    paste(x[1], x[2])
  } else if (length(x) >= 2) {
    # Standard behavior for names with at least two parts
    x[1]
  } else {
    # Standard behavior for names with only one part
    x
  }
})

Elite11Players$Last_Name <- sapply(name_parts, function(x) {
  # Check if the name has more than two parts and the first part ends with a period
  if (length(x) > 2 && grepl("\\.$", x[1])) {
    # Skip the first two parts and combine the rest
    paste(x[-c(1, 2)], collapse = " ")
  } else if (length(x) >= 2) {
    # Standard behavior for names with at least two parts
    paste(x[-1], collapse = " ")
  } else {
    # No last name if there's only one part
    ""
  }
})

#Elite11 Finalists 
Elite11Finalists <- Elite11Players %>%
  filter(!stringr::str_detect(Awards, "Elite")) #remove MVPs
#Elite11 MVPs
Elite11MVPs<- Elite11Players %>%
  filter(stringr::str_detect(Awards, "Elite")) #remove Finalists

#Remove In School athletes:
Elite11MVPs <- Elite11MVPs %>%
  filter(!stringr::str_detect(Draft, 'In School'))

Elite11Finalists <- Elite11Finalists %>%
  filter(!stringr::str_detect(Draft, 'In School'))


Drafted_Finalists <- Elite11Finalists%>%
  filter(!stringr::str_detect(Draft, "Undrafted"))
#------------------------------------------------------
# Calculate the total number of Elite11 finalists
total_finalists <- nrow(Elite11Finalists)

# Calculate the number of drafted finalists
drafted_finalists_count <- nrow(Drafted_Finalists)+2 #Account for TE's and 2024 draft

# Calculate the percentage of finalists who were drafted
finalist_percentage_drafted <- (drafted_finalists_count / total_finalists) * 100

# Output the result
finalist_percentage_drafted
  
total_MVPs <- nrow(Elite11MVPs) 

# Create a data frame for drafted MVPs by filtering out 'Undrafted'
Drafted_MVPs <- Elite11MVPs %>%
  filter(!stringr::str_detect(Draft, "Undrafted"))

# Calculate the number of drafted MVPs
drafted_MVPs_count <- nrow(Drafted_MVPs)

# Calculate the percentage of MVPs who were drafted
percentage_MVPs_drafted <- (drafted_MVPs_count / total_MVPs) * 100

# Output the result
percentage_MVPs_drafted  
#-----------------------------------------------------
#Same for 2010-2023
Elite11MVPs_after2010 <- Elite11MVPs %>%
  filter(Year>=2010)

after_2010_MVPs <- nrow(Elite11MVPs_after2010)

# Create a data frame for drafted MVPs by filtering out 'Undrafted'
Drafted_MVPs_after2010 <- Elite11MVPs_after2010 %>%
  filter(!stringr::str_detect(Draft, "Undrafted"))

# Calculate the number of drafted MVPs
drafted_MVPs_count2010 <- nrow(Drafted_MVPs_after2010) + 2 #Account for Rattler and Willams

# Calculate the percentage of MVPs who were drafted
percentage_MVPs_drafted2010 <- (drafted_MVPs_count2010 / after_2010_MVPs) * 100

# Output the result
percentage_MVPs_drafted2010

#-------------------------------------------------------
Elite11Finalists_after2010 <- Elite11Finalists %>%
  filter(Year>=2010)

after_2010_Finalists <- nrow(Elite11Finalists_after2010)

# Create a data frame for drafted MVPs by filtering out 'Undrafted'
Drafted_Finalists_after2010 <- Elite11Finalists_after2010 %>%
  filter(!stringr::str_detect(Draft, "Undrafted"))

# Calculate the number of drafted MVPs
drafted_Finalists_2010 <- nrow(Drafted_Finalists_after2010)+4

# Calculate the percentage of MVPs who were drafted
percentage_Finalists_drafted_2010 <- (drafted_Finalists_2010 / after_2010_Finalists) * 100

# Output the result
percentage_Finalists_drafted_2010
#--------------------------------------------------------
#Find average draft position for MVPs:
MVP_draft_position <- read_csv("MVP_draft_position.csv")
MVP_draft_position_2010 <- MVP_draft_position %>%
  filter(Elite11Class >= 2010)

avg_MVP_position <- mean(MVP_draft_position$`Draft Pick Overall`)
avg_MVP_position_2010 <-mean(MVP_draft_position_2010$`Draft Pick Overall`)
#Find average draft position for Finalists:

#Note COrnelius Ingram and Blake Bell were drafted as TE and are excluded.
best_finalist_year <- read_csv("best_finalist_year.csv")
best_finalist_year_2010 <- best_finalist_year %>%
  filter(Elite11Class >= 2010)
avg_finalist_position <- mean(best_finalist_year$`Draft Pick Overall`)
avg_finalist_position_2010 <- mean(best_finalist_year_2010$`Draft Pick Overall`)

best_finalist_year <- finalist_yearly %>%
  arrange(desc(`PPR Points`)) %>%
  distinct(Player, .keep_all = TRUE)
#write.csv(best_finalist_year, file = 'best_finalist_year.csv')

draft_summary <- data.frame(
  Category = c("MVPs", "Finalists", "MVPs (2010-2023)", "Finalists (2010-2023)"),
  Draft_Percentage = c(
    percentage_MVPs_drafted,
    finalist_percentage_drafted,
    percentage_MVPs_drafted2010,
    percentage_Finalists_drafted_2010
  ),
  Avg_Draft = c(
    avg_MVP_position,
    avg_finalist_position,
    avg_MVP_position_2010,
    avg_finalist_position_2010
  )
)
gt_table <- gt(draft_summary) %>%
  tab_header(
    title = html(
      paste0(
        "<table style='width: 100%;'><tr>",
        "<td style='text-align: left; vertical-align: middle;'>",
        "<span style='font-weight: bold;'>MVP & Finalist Draft Comparison </span>",
        "</td>",
        "<td style='text-align: right; vertical-align: middle;'>",
        "<img src='https://yt3.googleusercontent.com/QjzfaKc1XXbiKb2Yy-zqhuwilT3kqX3WbRrL7cMf8N-BlaFyXJszsEUmlcQ2amumom28QSXnyw=s900-c-k-c0x00ffffff-no-rj' style='height: 60px;' />",
        "</td>",
        "</tr></table>"
      )),
    subtitle = md("**What percentage of Elite 11 MVPs get drafted compared to Elite 11 Finalists?**")
  ) %>%
  cols_label(
    Category = "Category",
    Draft_Percentage = "Draft Percentage (%)",
    Avg_Draft = "Average Draft Position"
  ) %>%
  fmt_number(
    columns = c(Draft_Percentage,Avg_Draft),
    decimals = 2,
    suffixing = TRUE # Enable suffixing to add the percent sign
  ) %>%
  tab_options(
    stub.font.weight = "bold"
  )%>%
  tab_footnote(
    footnote = "Accounts for the 2024 NFL Draft Projections.",
    locations = cells_column_labels(Draft_Percentage)
    )%>%
  tab_footnote(
    footnote = "Excludes Blake Bell & Cornelius Ingram who were drafted as TEs.",
    locations = cells_body(
      columns = c(Draft_Percentage),
      rows = c(2)
    )
  )%>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(Draft_Percentage),
    palette = c('red3','yellow3','green3'),
    domain = NULL
  ) %>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(Avg_Draft),
    palette = c('green3','yellow3','red3'),
    domain = NULL
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Wikipedia & Campus2Canton || By: JJ Parker || @JParker1738')

print(gt_table)

