source("Setup/Init.R")
source("Setup/theme_master.R")

######################################
# overview over different markets ####
######################################

# create big table
`Market Overview` <- as.data.frame(matrix(NA, nrow = 16, ncol = 1))

names(`Market Overview`) <- "Market"

# for loop to get all the info
for (i in 1:length(comp_list)) {
  
  `Market Overview`$Market[i] <- comp_list[[i]]$`Game ID`
  
  `Market Overview`$`Sellers' Valuation`[i] <- comp_list[[i]]$`Sellers' valuation`
  
  `Market Overview`$`Buyers' Valuation`[i] <- comp_list[[i]]$`Buyers' valuation`
  
  `Market Overview`$`CER`[i] <- comp_list[[i]]$`Competitive equilibrium`
  
  `Market Overview`$`Maximum Gains of Trade`[i] <- comp_list[[i]]$`Maximum gains of trade`
  
  `Market Overview`$Sellers[i] <- nrow(comp_list[[i]]$Ranks$Sellers)
  
  `Market Overview`$Buyers[i] <- nrow(comp_list[[i]]$Ranks$Buyers)
  
  `Market Overview`$Size[i] <- comp_list[[i]]$`Market participants`
  
  `Market Overview`$`Out of Market`[i] <- ifelse(!is.na(comp_list[[i]]$`Out of market`$Sellers[1]),
                                                 length(comp_list[[i]]$`Out of market`$Sellers),
                                                 0) + ifelse(!is.na(comp_list[[i]]$`Out of market`$Buyers[1]),
                                                             length(comp_list[[i]]$`Out of market`$Buyers),
                                                             0)
  
  `Market Overview`$`Market Logic`[i] <- comp_list[[i]]$`Market logic`
  
  
}

# use stargazer to get table
stargazer(`Market Overview`, type = "html", summary = FALSE, header = F, out = "Visuals/Market Overview.html", 
          rownames = F)

##########################
# bids per Deal/round ####
##########################

# the tables have 160 rows, so i will use the average value per game to break it down
`Average Bids per Deal per Game` <- bids_per_deal %>%
  
  group_by(treatment, game) %>%
  
  summarise(
    `Average Bids` = mean(bids),
    `Max Bids` = max(bids),
    `Min Bids` = min(bids),
    `Average Trades` = mean(trades), 
    `Average Bids per Deal` = mean(`Bids per Deal`)
  ) %>%
  
  ungroup()

# now again stargazer to ouput a html table
stargazer(`Average Bids per Deal per Game`, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Average Bids per Deal per Game.html", 
          rownames = F)

#####################
# aggressiveness ####
#####################

# this table too is quite big, so I break it down, but this time by treatment and side

`Average Aggressiveness` <- aggressiveness %>%
  
  group_by(treatment, side) %>%
  
  summarise(
    Size = uniqLen(id),
    `Average Aggressiveness` = round(mean(Aggressiveness), 2),
    `Most aggressive` = max(Aggressiveness),
    `Least aggressive` = min(Aggressiveness),
    # as First bid most aggressive is a dummy variable, I convert it into numeric by multiplying with 1
    # this converts T values to 1 and F values to 0
    `First bid most aggressive` = round(mean(`First Bid most aggressive` * 1), 4),
    `Average relative Aggressiveness` = round(mean(`relative aggressiveness`), 4)
  ) %>%
  
  ungroup()

# stargazer
stargazer(`Average Aggressiveness`, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Average Aggressiveness.html", 
          rownames = F)

# note: the most aggressive bids were done by the sellers; however, even though the table might suggest otherwise
# there were actually several sellers who were bidding so aggressively and not a single seller all by him/herself

##################
# cer trading ####
##################

`Average CER Trading` <- cer_trading %>%
  
  group_by(treatment, side) %>%
  
  summarise(
    `Average Active Participants` = round(mean(`Active Participants`),2),
    `Average Bids in CER` = round(mean(`Bids in CER`),2),
    `Average Total Bids` = round(mean(`Total Bids`),2),
    `Average Bids in CER [%]` = round(mean(`Bids in CER Ratio`),4),
    `More actively Bidding in CER` = round(mean(`CER Bids Comparison`),4)
  ) %>%
  
  ungroup()

# note: it makes a huge difference whether I take the mean of average bids in CER Ratio or I calculate it
# based on the average bids in CER and average total bids. 




