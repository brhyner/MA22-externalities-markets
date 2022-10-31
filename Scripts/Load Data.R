source("Setup/Init.R")

##### Loading Data #####

########################
# Trading behavior #####
########################

trading_behavior <- read.csv("Daten/data_externalities.csv")

save(trading_behavior, file = "Daten/trading_behavior.rda")


###################
# new approach ####
###################

# create unique Game Id
trading_behavior$game_id <- paste(trading_behavior$treatment, trading_behavior$game, sep = "_")

# create vector for iteration
game_iterator <- unique(trading_behavior$game_id)

# remove duplicates
unique_id <- trading_behavior[!duplicated(trading_behavior$id),]

# create placeholder for final list
comp_list <- list(list())

# iterate through games to get Competitive equilibrium
for (i in 1:length(game_iterator)) {
  
  # reset out of market list
  `Out of market` <- list(NA, NA)
  names(`Out of market`) <- c("Sellers", "Buyers")
  
  # reset Ranks list (to have data for the geom_step() diagrams)
  `Ranks` <- list(NA, NA, NA)
  names(`Ranks`) <- c("Sellers", "Buyers", "Raw")
  
  # name iteration
  `Game ID` <- game_iterator[i]
  
  # filter by game id
  temp <- unique_id[unique_id$game_id == game_iterator[i],]
  
  # create tables and sort them
  # temporary seller table
  temp_seller <- filter(temp, side == "Seller") %>% 
    
    select(., game_id, side, valuation) %>%
    
    arrange(desc(valuation))
  
  # temporary buyer table
  temp_buyer <- filter(temp, side == "Buyer") %>%
    
    select(., game_id, side, valuation) %>%
    
    arrange(valuation)
  
  # what is the number of market participants
  `Market participants` <- nrow(temp_buyer) + nrow(temp_seller)
  
  # what is the market logic? (excess supply, excess demand or balanced)
  `Market logic` <- ifelse(nrow(temp_seller) > nrow(temp_buyer), 
                           "excess supply",
                           ifelse(nrow(temp_seller) < nrow(temp_buyer), 
                                  "excess demand", 
                                  "balanced"))

  # what are the valuations of the sellers
  `Sellers' valuation` <- paste(temp_seller$valuation, collapse = "; ")
  
  # what are the valuations of the buyers
  `Buyers' valuation` <- paste(temp_buyer$valuation, collapse = "; ")
  
  
  # depending on market logic, we drop sellers or buyers so they do not enter CE calculations
  # further, if max seller is higher than min buyer, both will be dropped to get a CE; all the dropped 
  # valuations enter the list of out of market valuations
  if(`Market logic` == "excess supply") {
    
    # get the difference
    diff <- nrow(temp_seller) - nrow(temp_buyer)
    
    # fill out of market List
    `Out of market`$Sellers <- temp_seller[1:diff,3]
    
    # remove the first diff rows
    temp_seller <- temp_seller[-(1:diff),]
    
    # reset while iterator
    w <- 0
    
    # check if max seller is higher than min buyer
    while (temp_seller[1,3] > temp_buyer[1,3]) {
      
      # how many times did this loop iterate
      w <- w + 1
      
      # append out of market list
      # as the out of market list already has diff number of arguments,
      # I need to take this into account in order not to overwrite the existing valuations
      `Out of market`$Sellers[diff+w] <- temp_seller[1,3]
      
      # remove first seller
      temp_seller <- temp_seller[-1,]
      
      # append out of market list
      # since there is an excess supply, the buyers out of market list is empty
      # therefore, I can start at 1
      `Out of market`$Buyers[w] <- temp_buyer[1,3]
      
      # remove first buyer
      temp_buyer <- temp_buyer[-1,]
      
    }
    
  }else if(`Market logic` == "excess demand"){
    
    # get the difference
    diff <- nrow(temp_buyer) - nrow(temp_seller)
    
    # fill out of market List
    `Out of market`$Buyers <- temp_buyer[1:diff,3]
    
    # remove first diff rows
    temp_buyer <- temp_buyer[-(1:diff),]
    
    # reset while iterator
    w <- 0
    
    # check if max seller is higher than min buyer
    while (temp_seller[1,3] > temp_buyer[1,3]) {
      
      # how many times did this loop iterate
      w <- w + 1
      
      # append out of market list
      # with excess demand, it is the other way around
      # here, the sellers list is empty, so I can start at 1
      `Out of market`$Sellers[w] <- temp_seller[1,3]
      
      # remove first seller
      temp_seller <- temp_seller[-1,]
      
      # append out of market list
      # but here I have to take into account the already existing arguments
      `Out of market`$Buyers[diff+w] <- temp_buyer[1,3]
      
      # remove first buyer
      temp_buyer <- temp_buyer[-1,]
      
    }
    
  }else{ # in case the market logic is balanced
    
    # reset while iterator
    w <- 0
    
    # check if max seller is higher than min buyer
    while (temp_seller[1,3] > temp_buyer[1,3]) {
      
      # how many times did this loop iterate
      w <- w + 1
      
      # append out of market list
      # out of market list is empty, so I can start at 1
      `Out of market`$Sellers[w] <- temp_seller[1,3]
      
      # remove first seller
      temp_seller <- temp_seller[-1,]
      
      # append out of market list
      # out of market list is empty, so I can start at 1
      `Out of market`$Buyers[w] <- temp_buyer[1,3]
      
      # remove first buyer
      temp_buyer <- temp_buyer[-1,]
      
    }
    
  }
  
  # get the upper and lower boundary of the competitive equilibrium price range
  `CER: upper boundary` <- temp_buyer[1,3]
  `CER: lower boundary` <- temp_seller[1,3]
  
  # what is the competitive equilibrium price range
  `Competitive equilibrium` <- paste(temp_seller[1,3], temp_buyer[1,3], sep = ":")
  
  # fill lists with ranks and IDs to create geom_step() diagrams
  `Ranks`$Sellers <- temp %>%
    
    filter(side == "Seller") %>%
    
    arrange(valuation) %>%
    
    mutate(
      rank = 1:n()
    )
  
  `Ranks`$Buyers <- temp %>%
    
    filter(side == "Buyer") %>%
    
    arrange(desc(valuation)) %>%
    
    mutate(
      rank = 1:n()
    )
    
  # combine both tables into one
  `Ranks`$Raw <- rbind(`Ranks`$Sellers, `Ranks`$Buyers)
  
  # get maximum gains of trade
  # easily doable by adding up the difference between sellers and buyers sorted by valuation
  `Maximum gains of trade` <- sum(temp_buyer$valuation - temp_seller$valuation)
  
  # get everything together into a list
  list_merged <- list(`Game ID`, `Market participants`,
                                   `Sellers' valuation`, `Buyers' valuation`, `Maximum gains of trade`,
                                   `Out of market`, `CER: upper boundary`, `CER: lower boundary`,
                                   `Ranks`, `Competitive equilibrium`, `Market logic`)
  
  # name list
  names(list_merged) <- c("Game ID", "Market participants",
                        "Sellers' valuation", "Buyers' valuation", "Maximum gains of trade",
                        "Out of market", "CER: upper boundary", "CER: lower boundary",
                        "Ranks", "Competitive equilibrium", "Market logic")
  
  # merge with other iterations
  comp_list[[i]] <- list_merged
  
}


##################
# Final Table ####
##################

# create placeholder for table
t_ce <- as.data.frame(matrix(NA, nrow = 16, ncol = 1))

# get relevant data from the comparison list
for (i in 1:length(comp_list)) {
  
  # get game id
  t_ce$`Game ID`[i] <- comp_list[[i]]$`Game ID`
  
  # get competitive equilibrium price range
  t_ce$`Competitive Equilibrium Price Range`[i] <- comp_list[[i]]$`Competitive equilibrium`
  
}

# remove NA column
t_ce <- t_ce[,-1]




##################################
# Identify out of market bids ####
##################################

# get unique id table
head(unique_id)

# add out of market dummy --> default to false
unique_id$`out of market` <- F

# loop through this table to find "out of market" bids
for (i in 1:nrow(unique_id)) {
  
  # I need a match on game Id, side and out of market valuation
  # I loop through the comparison list to get the out of market valuations
  # if there is a match, change out of market dummy to true
  for (j in 1:length(comp_list)) {
    unique_id$`out of market`[i] <- ifelse(
      
      # match on game id
      unique_id$game_id[i] == comp_list[[j]]$`Game ID` &&
        
      # check whether valuation is in the out of market list and whether the side matches
      ((unique_id$valuation[i] %in% comp_list[[j]]$`Out of market`$Sellers && unique_id$side[i] == "Seller") || 
       (unique_id$valuation[i] %in% comp_list[[j]]$`Out of market`$Buyers && unique_id$side[i] == "Buyer")
      ),
      T,
      unique_id$`out of market`[i]
    )
  }
  
  
  
}

# merge out of market dummy with trading behavior table
bids_oom <- merge(trading_behavior, select(unique_id, id, `out of market`))

#####################
# Identify ranks ####
#####################

# I already have the data saved under comp_list[[i]]$Ranks$Raw
# I just need to collapse into one dataframe
rawdata <- as.data.frame(matrix(NA, nrow = 1, ncol = 15))

# copy names from the dataframe in the comp list
names(rawdata) <- names(comp_list[[1]]$Ranks$Raw)

for (i in 1:length(comp_list)) {
  
  rawdata <- rbind(rawdata, comp_list[[i]]$Ranks$Raw)
  
}

# get rid of first row
rawdata <- rawdata[-1,]

# merge out of market dummy with trading behavior table
bids_ranks <- merge(bids_oom, select(rawdata, id, rank))

################
# save file ####
################

save(bids_ranks, file = "Daten/trading_behavior_ranks.rda")

save(comp_list, file = "Daten/Masterlist.rda")

save(rawdata, file = "Daten/unique_list.rda")

# save image
save.image()

# wipe everything clean
rm(list = ls())
