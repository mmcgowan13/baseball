if(!require("devtools")) install.packages("devtools")
library(devtools)
install_github("trinker/pacman")
pacman::p_load(tidyverse, mixtools, KScorrect)
install_github("mmcgowan13/baseballr")
# I forked Bill Petti's baseballr package to make some minimal changes.
library(baseballr)


# This assumes we have already saved Statcast data to a file in the below directory. I previously did so
# with the help of baseballr::scrape_statcast_savant.
allStatcastData  = readRDS(file = "C:\\Users\\Administrator\\Documents\\CASSIE\\cassie\\R\\allStatcastData.rds")

# For our purposes here we will remove bunts from consideration
statcastEV = allStatcastData %>% filter(!is.na(launch_speed) & !stringr::str_detect(des, "bunt"))

# For convenience we will do some lookups using first and last name, so we will do the lookup using a helpful
# table from baseballr that has MLBAM IDs and first and last names
chadwickLu = get_chadwick_lu()
players = chadwickLu %>% mutate(BatterName = paste(name_first, name_last, sep=" ")) %>% select(key_mlbam, BatterName)

# Now that we have a table with names like we want, we can join it to the Statcast data.
statcastEV = statcastEV %>% left_join(players, by=c("batter" = "key_mlbam"))

# We create a function that, given a player name, will fit a mixture of normal distributions to their exit
# velocity data. The main work is being done by normalmixEM from the mixtools package. It takes some
# initial conditions and uses the expectation maximization algorithm.
fitForPlayer = function(playerName){
  playerData = statcastEV %>% filter(BatterName == playerName)
  mixtools::normalmixEM(playerData$launch_speed, lambda=c(.8, .2), mu=c(80, 100), sigma=c(15, 5), epsilon = 1e-10)
  # The "magic numbers" for lambda, mu, and sigma represent initial conditions that are reasonable guesses from
  # which to start the EM algorithm. Here they represent an initial guess of weakly hit balls centered at 8 MPH
  # with a standard deviation of 15 and barrelled balls centered at 100 MPH with a standard deviation of 5. The
  # lambda parameter represents the mixture between the two, and the initial condition shows 80% weakly hit.
}

fitForMcCutchen = fitForPlayer("Andrew McCutchen")
fitForTrout = fitForPlayer("Mike Trout")

# Here are the important parameters returned
#> fitForMcCutchen$mu
#[1] 80.14761 99.53702
#> fitForMcCutchen$sigma
#[1] 13.091572  4.831506
#> fitForMcCutchen$lambda
#[1] 0.7136832 0.2863168

#> fitForTrout$mu
#[1]  80.14404 105.74765
#> fitForTrout$sigma
#[1] 13.644480  4.914679
#> fitForTrout$lambda
#[1] 0.8032977 0.1967023

plot.mixEM(fitForMcCutchen, whichplots = 2)
plot.mixEM(fitForTrout, whichplots = 2)
# The mixtools package provides the handy function plot.mixEM to visualize these
# distributions. The histograms are from the actual data, the green shows the
# well-struck piece of the normal mixture, and the red shows the not well-struck
# piece of the normal mixture.

estimatedMaxEVMcCutchen = KScorrect::qmixnorm(.99, mean=fitForMcCutchen$mu , sd=fitForMcCutchen$sigma, pro=fitForMcCutchen$lambda)
estimatedMaxEVTrout = KScorrect::qmixnorm(.99, mean=fitForTrout$mu , sd=fitForTrout$sigma, pro=fitForTrout$lambda)
# One use-case for getting the parameterization is to be able to infer "max" without the risks and noise
# of relying upon a single swing.
#> estimatedMaxEVMcCutchen
#[1] 110.6948
#> estimatedMaxEVTrout
#[1] 115.041
