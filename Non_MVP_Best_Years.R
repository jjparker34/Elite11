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
drafted_finalists_count <- nrow(Drafted_Finalists)

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
drafted_MVPs_count2010 <- nrow(Drafted_MVPs_after2010) + 2

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
drafted_Finalists_2010 <- nrow(Drafted_Finalists_after2010)

# Calculate the percentage of MVPs who were drafted
percentage_Finalists_drafted_2010 <- (drafted_Finalists_2010 / after_2010_Finalists) * 100

# Output the result
percentage_Finalists_drafted_2010
#--------------------------------------------------------



draft_summary <- data.frame(
  Category = c("MVPs", "Finalists", "MVPs (2010-2023)", "Finalists (2010-2023)"),
  Draft_Percentage = c(
    percentage_MVPs_drafted,
    finalist_percentage_drafted,
    percentage_MVPs_drafted2010,
    percentage_Finalists_drafted_2010
  )
)
gt_table <- gt(draft_summary) %>%
  tab_header(
    title = html(
      paste0(
        "<img src='https://highschoolfootballamerica.com/wp-content/uploads/2020/06/elite-11-scaled.jpg' style='height: 60px;' />",
        "<br>",
        "<span style='font-weight: bold;'>MVP and Finalists Draft Comparison</span>"
      )),
    subtitle = md("**What percentage of Elite 11 MVPs get drafted compared to Elite 11 Finalists?**")
  ) %>%
  cols_label(
    Category = "Category",
    Draft_Percentage = "Draft Percentage (%)"
  ) %>%
  fmt_number(
    columns = c(Draft_Percentage),
    decimals = 2,
    suffixing = TRUE # Enable suffixing to add the percent sign
  ) %>%
  text_transform(
    locations = cells_stub(),
    fn = function(x) {
      ifelse(x %in% c("MVPs", "Finalists"), html("<strong>", x, "</strong>"), x)
    }
  )%>%
  tab_options(
    stub.font.weight = "bold"
  )%>%
  tab_footnote(
    footnote = "Accounts for Spencer Rattler & Caleb Williams Draft Capital",
    locations = cells_body(
      columns = c(Category),rows = c(1,3) 
    )
  )%>%
  opt_table_font(font = google_font('Lato')) %>%
  data_color(
    columns = c(Draft_Percentage),
    palette = c('red3','yellow3','green3'),
    domain = NULL
  ) %>%
  opt_align_table_header(align = 'left') %>%
  tab_source_note(source_note = 'Data is from Wikipedia || By: JJ Parker || @JParker1738')

gt_table




#CFBFastR

cfbd_stats_season_player(2019, team = "LSU", category = "passing")
