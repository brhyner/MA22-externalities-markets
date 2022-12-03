source("Setup/Init.R")

###################
# Loading Data #########

load("Daten/trading_behavior_ranks.rda")

load("Daten/Masterlist.rda")

load("Daten/unique_list.rda")

###################
########################
# Trading behavior #####
########################

# transfer all the aggregate level stuff from the master list on the trading (bids_ranks) df
# initialize missing columns
bids_ranks$`Market participants` <- NA
bids_ranks$`Market logic` <- NA
for (i in 1:length(comp_list)) {
  
  # market participants
  bids_ranks$`Market participants`[bids_ranks$game_id == comp_list[[i]]$`Game ID`] <-
    comp_list[[i]]$`Market participants`
  
  # market logic
  bids_ranks$`Market logic`[bids_ranks$game_id == comp_list[[i]]$`Game ID`] <-
    comp_list[[i]]$`Market logic`
  
}

# the rest will be continuously added throughout the code

#############
# In CER ####
#############
# merge CER upper and lower boundary into bids list
mergetable <- as.data.frame(matrix(NA, nrow = 16, ncol = 3))

# loop through masterlist and unique list
for (j in 1:length(comp_list)) {
  
  mergetable$game_id[j] <- comp_list[[j]]$`Game ID`
  mergetable$cer_upper[j] <- comp_list[[j]]$`CER: upper boundary`
  mergetable$cer_lower[j] <- comp_list[[j]]$`CER: lower boundary`
  
}

mergetable <- mergetable[,-(1:3)]

# merge this into bids list
trading <- merge(mergetable, bids_ranks)

# add a dummy for whether Bid was in CE Range
trading$`In CER` <- F

# loop through bids data and update cer dummy
for (i in 1:nrow(trading)) {
  
    trading$`In CER`[i] <- ifelse(
      
      trading$bid[i] >= trading$cer_lower[i] && trading$bid[i] <= trading$cer_upper[i],
      T,
      trading$`In CER`[i]
      
    )
  
}

# some stats about CER bids and trades
cer_trading_ratio <- trading %>%
  
  group_by(treatment, game, round, side, `In CER`) %>%
  
  summarise(
    Frequency = n()
  ) %>%
  
  group_by(treatment, game, round, side) %>%
  
  mutate(
    `Bids in CER Ratio` = round(Frequency / sum(Frequency), 2),
    `Total Bids` = sum(Frequency),
    identifier = paste0(treatment, game, round, side)
  ) %>%
  
  ungroup()

#öaklsdjf in some rounds, all bids fell outside the CER

# which side is more actively bidding in the CER
cer_trading <- trading %>%
  
  filter(`In CER` == T) %>%
  
  group_by(treatment, game, round, side) %>%
  
  summarise(
    `Bids in CER` = n(),
    `Active Participants` = uniqLen(id)
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  mutate(
    # which side is more active within the CER
    `CER Bids Comparison` = round(`Bids in CER` / sum(`Bids in CER`), 2),
    identifier = paste0(treatment, game, round, side)
  ) %>%
  
  ungroup()

# I would like to have the number of total bids in this table as well
# if I do not filter the second table, the merge will not work, since the merge would try to match the nrow
# of the second table // with filtering, I get the same nrow!
cer_trading <- merge(x = cer_trading, y = select(cer_trading_ratio[cer_trading_ratio$`In CER`==T,], 
                                          identifier, `Total Bids`, `Bids in CER Ratio`), all.x = T) %>%
  
  select(., -identifier)

##############
# Surplus ####
##############

# what are the realized gains of trade and how does it relate to the maximum gains of trade
surplus <- trading %>%
  
  group_by(treatment, game, round) %>%
  
  summarize(
    surplus = sum(abs(price - valuation), na.rm = T)
  ) %>%
  
  mutate(
    # to create key to match with masterlist
    `Game ID` = paste(treatment, game, sep = "_")
  ) %>%
  
  ungroup()
  
# join with masterlist and add Maximum Gains of trade
surplus$`Maximum gains of trade` <- NA

# create loop for masterlist
for (i in 1:length(comp_list)) {
  
  surplus$`Maximum gains of trade`[surplus$`Game ID` == comp_list[[i]]$`Game ID`] <- 
    comp_list[[i]]$`Maximum gains of trade`
  
}

# now define allocative efficiency by dividing realized gains of trade by maximum gains of trade
surplus$`Allocative efficiency` <- round(surplus$surplus / surplus$`Maximum gains of trade`, 4)

#################################
# Relative Surplus by Group #####
#################################

# relative surplus is the relative difference of the surplus between the two groups. 
# it shows which group benefitted more from the transactions.

relative_surplus <- trading %>%
  
  group_by(treatment, game, round, side) %>%
  
  summarize(
    surplus = sum(abs(price - valuation), na.rm = T)
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  mutate(
    relative_surplus = round(surplus / sum(surplus), 2)
  ) %>%
  
  ungroup()



######################################
# Trading Volume Number of trades ####
######################################

trading_volume <- trading[!is.na(trading$price),] %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    # since there are always two sides to a trade, I divide the count by two in order to not 
    # count trades twice
    `Number of trades` = n() / 2
  ) %>%
  
  mutate(
    identifier = paste(treatment, game, sep = "_")
  ) %>%
  
  ungroup()

# what would be max possible trades
# use same mergetable logic from above
mergetable <- as.data.frame(matrix(NA, nrow = 16, ncol = 2))

# loop through masterlist
for (j in 1:length(comp_list)) {
  
  # first create identifier to later merge with the trading volume table
  mergetable$identifier[j] <- comp_list[[j]]$`Game ID`
  
  # get row number of sellers and buyers data frames
  # IMPORTANT - Does include OUT OF MARKET participants
  mergetable$`Maximum number of trades`[j] <- min(
    nrow(comp_list[[j]]$Ranks$Sellers), 
    nrow(comp_list[[j]]$Ranks$Buyers)
  )
  
}

mergetable <- mergetable[,-(1:2)]

# merge into trading volume data frame and calculate trades ratio
trading_volume <- merge(trading_volume, mergetable) %>%
  
  mutate(
    `Possible Trades %` = round(`Number of trades` / `Maximum number of trades`, 4)
  )

####################
# Time Measures ####
####################

# convergence
# how long does it take the market to converge
mergetable <- trading %>%
  
  filter(`In CER` == T) %>%
  
  group_by(treatment, game, round, side) %>%
  
  summarise(
    cer_convergence = min(time),
    cer_bids = n(),
    .groups = "rowwise"
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  mutate(
    # which side is more active within the CER
    cer_bid_ratio = round(cer_bids / sum(cer_bids), 2), 
    # create an identifier that I can use to loop through the table
    identifier = paste0(treatment, game, round, side) 
  ) %>%
  
  ungroup()

# what are other non-cer specific characteristics on game/round level
convergence_general <- trading %>%
  
  group_by(treatment, game, round, side) %>%
  
  summarise(
    first_bid = min(time),
    last_bid = max(time),
    bidding_duration = last_bid - first_bid,
    participants = length(unique(id)), 
    bids = n()
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  mutate(
    # which side is more active within the CER
    bid_ratio = round(bids / sum(bids), 2),
    # create an identifier that I can use to loop through the table
    identifier = paste0(treatment, game, round, side)
  ) %>%
  
  ungroup()

# now combine the convergence general table with the convergence speed table
# in order to get the whole view of the data
# to do this, I need an outer join ("all" parameter in merge command set to T)
convergence_general <- merge(convergence_general, select(mergetable, 
                                                 cer_convergence, cer_bids, cer_bid_ratio, identifier), 
                             all = T) %>%
  
  mutate(
    `Time between Convergence and first Bid` = cer_convergence - first_bid,
    `Time passed to convergence in %` = round(`Time between Convergence and first Bid` / bidding_duration, 4)
  )

# avg time per bid
time_per_bid <- trading %>%
  
  group_by(treatment, game, round, side) %>%
  
  summarise(
    first_bid = min(time),
    last_bid = max(time),
    bidding_duration = last_bid - first_bid,
    participants = length(unique(id)), 
    bids = n()
  ) %>%
  
  mutate(
    `Time per bid` = round(bidding_duration / bids, 2)
  ) %>%
  
  ungroup()
  
# avg time per trade
time_per_trade <- trading %>%
  
  filter(!is.na(price)) %>%
  
  # I do not need to group by side, as this is going to be the same for both on an aggregate level
  # (because for each trade, both sides are involved)
  group_by(treatment, game, round) %>%
  
  summarise(
    first_trade = min(match_time),
    last_trade = max(match_time),
    trading_duration = last_trade - first_trade,
    # n() needs to be divided by 2, otherwise it counts each trade twice, 
    # as the raw data contains both the seller and the buyer side
    trades = n()/2
  ) %>%
  
  mutate(
    `Time per trade` = round(trading_duration / trades, 2)
  ) %>%
  
  ungroup()

# I would like to include max trades in order to see whether the market was cleared or not // should I though?

####################
# Bids per Deal ####
####################

# how many bids were there per round?
bids_per_round <- trading %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    bids = n()
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()

# how many trades?
trades_per_round <- trading %>%
  
  filter(
    !is.na(price)
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    # n() needs to be divided by 2, otherwise it counts each trade twice, 
    # as the raw data contains both the seller and the buyer side
    trades = n()/2
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()
  
# merge dataframes to calculate bids per deal
bids_per_deal <- merge(bids_per_round, select(trades_per_round, 
                                              identifier, trades)) %>%
  
  select(., -identifier) %>%
  
  mutate(
    `Bids per Deal` = round(bids / trades, 2)
  ) %>%
  
  group_by(treatment, game) %>%
  
  summarise(
    avg_bids = mean(bids),
    avg_trades = mean(trades),
    avg_bpd = mean(`Bids per Deal`)
  ) %>%
  
  ungroup()
  
#####################
# Aggressiveness ####
#####################

# aggressiveness is the difference between bid and valuation
# especially interesting is the first bid
# the aggressiveness of the first bid can then be compared with the most aggressive bid of 
# a participant in this round

# overall aggressiveness
aggressiveness <- trading %>%
  
  # since this is an individual level statistic, I group by ID too
  group_by(treatment, game, round, side, id) %>%
  
  summarise(
    `Aggressiveness` = max(abs(bid - valuation))
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round, side, id)
  ) %>%
  
  ungroup()


# in order to figure out the aggressiveness of the first bid, I need a loop
# specifically, I loop through the ids from the aggressiveness table
# first though, I need a matching key in the trading table // after the loop, I will delete it as I 
# do not need it anymore
placeholder <- trading %>%
  
  mutate(
    identifier = paste0(treatment, game, round, side, id)
  )

# also initialise the first bid aggressiveness, valuation and the out of market column
aggressiveness$`First Bid Aggressiveness` = NA
aggressiveness$valuation = NA
aggressiveness$`out of market` = NA

# looping!
for (i in 1:nrow(aggressiveness)) {
  
  # here I need to find the first bid; but the calculation remains the same
  aggressiveness$`First Bid Aggressiveness`[i] = abs(
    
    # first identify the time of the first bid and then match on this time as well as the identifier
    placeholder$bid[placeholder$time == min(placeholder$time[placeholder$identifier == aggressiveness$identifier[i]]) & 
                      placeholder$identifier == aggressiveness$identifier[i]] - 
      
      # then the valuation
      # here I have to make sure that even though it might be a vector with length > 1
      # I only take the first one
      # the reason this might be a vector of length > 1 is that the identifier is not unique for bids!
      placeholder$valuation[placeholder$identifier == aggressiveness$identifier[i]][1]
    
  )
  
  # since it might be interesting to have this as well, let's also include the out of market dummy variable
  # same thing with the vector of length > 1
  aggressiveness$`out of market`[i] = 
    placeholder$`out of market`[placeholder$identifier == aggressiveness$identifier[i]][1]
  
  # I can also add the valuation in order to calculate a measure for the relative aggressive
  # which is the aggressiveness divided by valuation
  aggressiveness$valuation[i] = 
    placeholder$valuation[placeholder$identifier == aggressiveness$identifier[i]][1]
  
}

# in some cases, the first bid is not as aggressive as other bids
aggressiveness$`First Bid most aggressive` = 
  ifelse(
    aggressiveness$`First Bid Aggressiveness` >= aggressiveness$Aggressiveness,
    T,
    F
    )

# relative aggressiveness is calculated as the division of the aggressiveness and valuation
# the higher the number, the more aggressive a bid (and in that case participant) was
aggressiveness$`relative aggressiveness` = round(aggressiveness$Aggressiveness / aggressiveness$valuation, 4)

# we do not need the placeholder anymore
rm(placeholder)

#############################
# Out of Market Activity ####
#############################

# how active are participants who were designed to be out of market
out_of_market_bids <- trading %>%
  
  filter(`out of market` == T) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    `out of market bids` = n()
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()


# calculate generic table with total bids to later merge with out of market table
mergetable <- trading %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    `total bids` = n()
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()
  
# merge with other table
out_of_market_bids <- merge(mergetable, select(out_of_market_bids, 
                                             identifier, `out of market bids`), 
                       all = T) %>%
  
  mutate(
    `out of market bids` = ifelse(
      is.na(`out of market bids`), 0, `out of market bids`),
    `out of market ratio` = round(`out of market bids` / `total bids`, 4)
    ) %>%
  
  select(., -identifier)

# were there any trades involving out of market participants?
out_of_market_trades <- trading %>%
  
  filter(!is.na(price) &
           `out of market` == T) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    `out of market trades` = n()
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()

# again using the mergetable placeholder to get a ratio
mergetable <- trading %>%
  
  filter(!is.na(price)) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    # n() needs to be divided by 2, otherwise it counts each trade twice, 
    # as the raw data contains both the seller and the buyer side
    `Total trades` = n()/2
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()

# merge!
out_of_market_trades <- merge(mergetable, select(out_of_market_trades, 
                                                 identifier, `out of market trades`), 
                              all = T) %>%
  
  mutate(
    `out of market trades` = ifelse(
      is.na(`out of market trades`), 0, 
      `out of market trades`
    ), 
    `OOM Ratio` = round(`out of market trades` / `Total trades`, 4)
  )


# now I could merge this with the surplus table!
# first I need to create the identifier column though
surplus <- surplus %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  )

# now I can merge it with the out of market table
out_of_market_trades <-  merge(out_of_market_trades, select(surplus, 
                                                            identifier, 
                                                            surplus, 
                                                            `Maximum gains of trade`, 
                                                            `Allocative efficiency`))
  
  

# above I also have the aggressiveness as a characteristic of out of market participants
summary(aggressiveness$`out of market`)

##################################################
# distribution of activity among participants ####
##################################################

########################
# Tidy up the space ####
########################
# remove the things I do not need anymore
# also later I can load the image directly to get the necessary table to do actual analysis and visualizations
rm(
  list = c("bids_ranks", "mergetable", "rawdata", "comp_list", "i", "j")
)

# save trading master df
save(trading, file = "Daten/trading_master.rda")

# save image
save.image("Daten/Data Prep - Image.RData")

