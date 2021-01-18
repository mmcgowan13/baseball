if(!require("devtools")) install.packages("devtools")
library(devtools)
install_github("trinker/pacman")
pacman::p_load(tidyverse, mixtools, KScorrect)
install_github("mmcgowan13/baseballr")
# I forked Bill Petti's baseballr package to make some minimal changes
library(baseballr)


allStatcastData  = readRDS(file = "C:\\Users\\Administrator\\Documents\\CASSIE\\cassie\\R\\allStatcastData.rds")
statcastEV = allStatcastData %>% filter(!is.na(launch_speed) & !stringr::str_detect(des, "bunt"))

chadwickLu = get_chadwick_lu()
lahmanRetroPeople = Lahman::People %>% filter(!is.na(retroID)) %>% left_join(chadwickLu %>% select("key_retro", "key_mlbam"), by=c("retroID" = "key_retro"))

players = lahmanRetroPeople %>% mutate(BatterName = paste(nameFirst, nameLast, sep=" ")) %>% select(key_mlbam, BatterName)
statcastEV = statcastEV %>% left_join(players, by=c("batter" = "key_mlbam"))

fitForPlayer = function(playerName){
  playerData = statcastEV %>% filter(BatterName == playerName)
  mixtools::normalmixEM(playerData$launch_speed, lambda=c(.8, .2), mu=c(80, 100), sigma=c(15, 5), epsilon = 1e-10)
  # The "magic numbers" for lambda, mu, and sigma represent initial conditions that are reasonable guesses from
  # which to start the EM algorithm. Here they represent an initial guess of weakly hit balls centered at 8 MPH
  # with a standard deviation of 15 and barrelled balls centered at 100 MPH with a standard deviation of 5. The
  # lambda parameter represents the mixture between the two, and the initial condition shows 80% weakly hit.
}