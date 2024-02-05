library(tidyverse)
library(cfbfastR)

Sys.setenv(CFBD_API_KEY = "S2JgCsvJ5RPuw2dDwPquZ1xTKefAk5UT/H0qeK9jZAAH46WMsGbKtfRJ4odTswoX")
Elite11Players <- read_csv("Elite11Players.csv")


#Elite 11 MVPs
MVPs <- Elite11Players %>%
  filter(stringr::str_detect(Awards, "Elite"))

MVPs_after_2010 <- MVPs %>%
  filter(Year >= 2010)


#player_info <- cfbd_player_info(search_term = player_name, position = position, team = team, year = year)
#cfbd_player_info(search_term = "Berlin", position = "QB", team = "Florida", year = 2001)
