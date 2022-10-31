source('Setup/Init.R')
# macro Level ####

##################

# trading volume
summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])
sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])
sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])

# make sure that levels are correct

trading_volume$treatment <- factor(
  trading_volume$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNeg'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtNeg'], alternative = 'l')

# negative ext
wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtPos'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtPos'], alternative = 'g')

##################

# gains of trade
summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])
sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])
sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])

# make sure that levels are correct

surplus$treatment <- factor(
  surplus$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNeg'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtNeg'], alternative = 'g')

# negative ext
wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtPos'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtPos'], alternative = 'g')

##################

# price volatility
summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])
sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])
sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])

# make sure that levels are correct

price_volatility$treatment <- factor(
  price_volatility$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNeg'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtNeg'], alternative = 'l')

# negative ext
wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtPos'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtPos'], alternative = 'g')

################

# price levels
summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])
sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])
sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
length(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
length(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
length(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])

# make sure that levels are correct

price_levels$treatment <- factor(
  price_levels$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNeg'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtNeg'], alternative = 'l')

# negative ext
wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtPos'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtPos'], alternative = 'l')

#######################

# micro level #########

#######################

# aggressiveness

# buyers
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Buyer'])
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Buyer'])
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Buyer'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Buyer'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Buyer'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Buyer'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Buyer'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Buyer'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Buyer'])

# make sure that levels are correct

aggressiveness$treatment <- factor(
  aggressiveness$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg' & aggressiveness$side == 'Buyer'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg' & aggressiveness$side == 'Buyer'], alternative = 'g')

# negative ext
wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos' & aggressiveness$side == 'Buyer'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos' & aggressiveness$side == 'Buyer'], alternative = 'g')

########

# aggressiveness

# sellers
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Seller'])
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Seller'])
summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Seller'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Seller'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Seller'])
sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Seller'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & aggressiveness$side == 'Seller'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & aggressiveness$side == 'Seller'])
length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & aggressiveness$side == 'Seller'])

# make sure that levels are correct

aggressiveness$treatment <- factor(
  aggressiveness$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg' & aggressiveness$side == 'Seller'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg' & aggressiveness$side == 'Seller'], alternative = 'l')

# negative ext
wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos' & aggressiveness$side == 'Seller'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos' & aggressiveness$side == 'Seller'], alternative = 'l')

# truncate aggressiveness to get a better graph
aggressiveness_trunc <- aggressiveness %>%
  
  mutate(
    truncated_aggressiveness = ifelse(
      aggressiveness$`relative aggressiveness` > 1, 1, aggressiveness$`relative aggressiveness`
    )
  )

gg_aggressiveness_sellers_trunc <- ggplot(aggressiveness_trunc[aggressiveness_trunc$side == 'Seller',], 
                                    aes(x = treatment, y = truncated_aggressiveness)) + 
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  scale_y_continuous(labels = scales::percent) + 
  
  labs(x = 'Treatment', y = 'Relative Aggressiveness (truncated)') + theme_master()

gg_aggressiveness_sellers_trunc

ggsave("Visuals/Aggressiveness sellers truncated.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness sellers truncated.png", 
       width = 34.3, height = 22, units = "cm")
