### Library ----------------------------------------------------

library(tidyverse)

### Importation ------------------------------------------------

players <- read_csv("tennis_atp/atp_players.csv", col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")

simple <- read_csv("tennis_atp/atp_matches_2012.csv")

doubles <- read_csv("tennis_atp/atp_matches_doubles_2012.csv")

futures <- read_csv("tennis_atp/atp_matches_futures_2012.csv")

qual_chall <- read_csv("tennis_atp/atp_matches_qual_chall_2012.csv")

rankings <- read_csv("tennis_atp/atp_rankings_10s.csv")
