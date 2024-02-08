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

# Find MVP's best PPR year

highest_ppr_year <- MVP_stats_yearly %>%
  group_by(Player) %>%
  top_n(1, `PPR Points`) %>%
  select(Player,Team,Season,`Year out of HS`,`Games Played`,`PPR Points`,`PPR Points per Game`,`Draft Pick Overall`)


highest_ppr_year %>%
  gt() %>%
  tab_header(
    title = md('***Elite 11 MVPs Best Year (2010-2023)***'),
    subtitle = md('Elite 11 MVP and their best year in college stats based on ppr points')
  ) %>%
  opt_align_table_header(align = 'left') 
     