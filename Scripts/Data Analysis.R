source("Setup/Init.R")
source('Setup/theme_master.R')

# load Data ####
load("Daten/Data Prep - Image.RData")

# t-test family
# wilcoxon non-parametric version of paired t-test
# mann whitney non-parametric version of unpaired t-test --> this is the version I am using
# comparison between groups as opposed to relationships between variables
# also, categorical as well as quantitative data is compared which means the t-test family is the way to go

# non-parametric means that no assumptions were made regarding the distribution of the data

##################
# Macro Level ####
##################

# Trading Volume ####

gg_trading_volume <- ggplot(trading_volume, aes(x = treatment, y = `Possible Trades %`)) +
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  
  labs(x = 'Treatment', y = 'Trading Volume in %') + theme_master()

gg_trading_volume

ggsave("Visuals/Trading Volume.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Trading Volume.png", 
       width = 34.3, height = 22, units = "cm")

# new vis: taking rounds into consideration

trading_volume_rounds <- trading_volume %>%
  
  mutate(
    beginning = ifelse(
      round <= 3, 'Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'
    )
  ) %>%
  
  group_by(treatment, beginning) %>%
  
  summarise(
    relative_trading_volume = mean(`Possible Trades %`),
    sd = sd(`Possible Trades %`)
  ) %>%
  
  ungroup() %>%
  
  mutate(
    upper_bound = ifelse(relative_trading_volume + (sd*1.96) > 1, 
                         1, 
                         relative_trading_volume + (sd*1.96)),
    lower_bound = relative_trading_volume - (sd*1.96),
    beginning = factor(beginning, levels = c('Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'))
  ) 

gg_trading_volume_point_range <- ggplot(trading_volume_rounds,
                                        aes(x = beginning, y = relative_trading_volume)) + 
  
  geom_pointrange(aes(y = relative_trading_volume, ymin = lower_bound, ymax = upper_bound, color = treatment), 
                  position = position_dodge2(width = .3),
                  size = .6) +
  
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_color_brewer(palette = 'Blues') +
  
  labs(x = 'Start vs. Rest of Game', y = 'Trading Volume in %', color = 'Treatment Condition') + theme_master()

gg_trading_volume_point_range

ggsave("Visuals/Trading Volume Start of Game.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Trading Volume Start of Game.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test for difference between FullExtNorm and FullExtPos
wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNeg'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtNeg'])

# wilcoxon test for difference between FullExtNorm and FullExtNeg
wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtPos'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtPos'])

# wilcoxon test for difference between FullExtNeg and FullExtPos
wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNorm'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtNorm']) -> test_object_wilcox

`Control Group` <- c(
  NA, 
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtPos'] ~ 
                      trading_volume$treatment[trading_volume$treatment != 'FullExtPos'])$p.value,
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNeg'] ~ 
                trading_volume$treatment[trading_volume$treatment != 'FullExtNeg'])$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtPos'] ~ 
                trading_volume$treatment[trading_volume$treatment != 'FullExtPos'])$p.value,
  NA,
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNorm'] ~ 
                trading_volume$treatment[trading_volume$treatment != 'FullExtNorm'])$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNeg'] ~ 
               trading_volume$treatment[trading_volume$treatment != 'FullExtNeg'])$p.value,
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNorm'] ~ 
                trading_volume$treatment[trading_volume$treatment != 'FullExtNorm'])$p.value,
  NA
)

da_trading_volume_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_trading_volume_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_trading_volume_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Trading Volume Wilcoxon.html", 
          rownames = T)

# Gains of trade ####
gg_gains_of_trade <- ggplot(surplus, aes(x = treatment, y = `Allocative efficiency`)) +
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  
  labs(x = 'Treatment', y = 'Allocative Efficiency in %') + theme_master()

gg_gains_of_trade

ggsave("Visuals/Gains of Trade.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Gains of Trade.png", 
       width = 34.3, height = 22, units = "cm")

# same here: check whether the beginning paints a clearer picture

gains_of_trade_rounds <- surplus %>%
  
  mutate(
    beginning = ifelse(
      round <= 3, 'Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'
    )
  ) %>%
  
  group_by(treatment, beginning) %>%
  
  summarise(
    relative_gains_of_trade = mean(`Allocative efficiency`),
    sd = sd(`Allocative efficiency`)
  ) %>%
  
  ungroup() %>%
  
  mutate(
    upper_bound = ifelse(relative_gains_of_trade + (sd*1.96) > 1, 
                         1, 
                         relative_gains_of_trade + (sd*1.96)),
    lower_bound = relative_gains_of_trade - (sd*1.96),
    beginning = factor(beginning, levels = c('Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'))
  ) 

gg_gains_of_trade_point_range <- ggplot(gains_of_trade_rounds,
                                        aes(x = beginning, y = relative_gains_of_trade)) + 
  
  geom_pointrange(aes(y = relative_gains_of_trade, ymin = lower_bound, ymax = upper_bound, color = treatment), 
                  position = position_dodge2(width = .3),
                  size = .6) +
  
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_color_brewer(palette = 'Blues') +
  
  labs(x = 'Start vs. Rest of Game', y = 'Allocative Efficiency %', color = 'Treatment Condition') + theme_master()

gg_gains_of_trade_point_range

ggsave("Visuals/Gains of Trade Start of Game.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Gains of Trade Start of Game.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test for difference between FullExtNorm and FullExtPos
wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNeg'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtNeg'])

# wilcoxon test for difference between FullExtNorm and FullExtNeg
wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtPos'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtPos'])

# wilcoxon test for difference between FullExtNeg and FullExtPos
wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNorm'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtNorm'])

`Control Group` <- c(
  NA, 
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtPos'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtPos'])$p.value,
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNeg'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtNeg'])$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtPos'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtPos'])$p.value,
  NA,
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNorm'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtNorm'])$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNeg'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtNeg'])$p.value,
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNorm'] ~ 
                surplus$treatment[surplus$treatment != 'FullExtNorm'])$p.value,
  NA
)

da_surplus_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_surplus_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_surplus_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Gains of Trade Wilcoxon.html", 
          rownames = T)

# price volatility ####
# in order to calculate price volatility, I use the standard deviation of price and multiply it by  
# the square root of the number of trades

# first the sd of price per round, game, and treatment

price_volatility <- trading %>%
  
  filter(
    !is.na(price)
  ) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    sd = sd(price)
  ) %>%
  
  mutate(
    identifier = paste0(treatment, game, round)
  ) %>%
  
  ungroup()

# merge with trades per round to get number of trades

price_volatility <- merge(price_volatility, select(trades_per_round, identifier, trades)) %>%
  
  select(., -identifier) %>%
  
  mutate(
    price_volatility = round(sd * sqrt(trades), 2)
  )

gg_price_volatility <- ggplot(price_volatility, aes(x = treatment, y = price_volatility)) +
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  labs(x = 'Treatment', y = 'Price Volatility in \u03C3 of Price') + theme_master()

gg_price_volatility

ggsave("Visuals/Price Volatility.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Price Volatility.png", 
       width = 34.3, height = 22, units = "cm")

# check whether the beginning of each game is different compared to the rest of the game
price_volatility_rounds <- price_volatility %>%
  
  mutate(
    beginning = ifelse(
      round <= 3, 'Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'
    )
  ) %>%
  
  group_by(treatment, beginning) %>%
  
  summarise(
    average_price_volatility = mean(price_volatility),
    sd = sd(price_volatility)
  ) %>%
  
  ungroup() %>%
  
  mutate(
    upper_bound = average_price_volatility + (sd*1.96),
    lower_bound = ifelse(average_price_volatility - (sd*1.96) < 0,
                         0,
                         average_price_volatility - (sd*1.96)),
    beginning = factor(beginning, levels = c('Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'))
  )

gg_price_volatility_point_range <- ggplot(price_volatility_rounds,
                                        aes(x = beginning, y = average_price_volatility)) + 
  
  geom_pointrange(aes(y = average_price_volatility, ymin = lower_bound, ymax = upper_bound, color = treatment), 
                  position = position_dodge2(width = .3),
                  size = .6) +
  
  scale_color_brewer(palette = 'Blues') +
  
  labs(x = 'Start vs. Rest of Game', y = 'Average Price Volatility', color = 'Treatment Condition') + theme_master()

gg_price_volatility_point_range

ggsave("Visuals/Price Volatility Start of Game.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Price Volatility Start of Game.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test for difference between FullExtNorm and FullExtPos
wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNeg'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtNeg'])

# wilcoxon test for difference between FullExtNorm and FullExtNeg
wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtPos'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtPos'])

# wilcoxon test for difference between FullExtNeg and FullExtPos
wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNorm'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtNorm'])

`Control Group` <- c(
  NA, 
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtPos'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtPos'])$p.value,
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNeg'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtNeg'])$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtPos'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtPos'])$p.value,
  NA,
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNorm'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtNorm'])$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNeg'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtNeg'])$p.value,
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNorm'] ~ 
                price_volatility$treatment[price_volatility$treatment != 'FullExtNorm'])$p.value,
  NA
)

da_price_volatility_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_price_volatility_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_price_volatility_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Price Volatility Wilcoxon.html", 
          rownames = T)

# price levels ####
price_levels <- trading %>%
  
  filter(!is.na(price)) %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    price_levels = mean(price, na.rm = T)
  ) %>%
  
  ungroup()

gg_price_levels <- ggplot(price_levels, aes(x = treatment, y = price_levels)) +
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  labs(x = 'Treatment', y = 'Average Price Level') + theme_master()

gg_price_levels

ggsave("Visuals/Price levels.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Price levels.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test for difference between FullExtNorm and FullExtPos
wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNeg'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtNeg'])

# wilcoxon test for difference between FullExtNorm and FullExtNeg
wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtPos'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtPos'])

# wilcoxon test for difference between FullExtNeg and FullExtPos
wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNorm'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtNorm'])

`Control Group` <- c(
  NA, 
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtPos'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtPos'])$p.value,
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNeg'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtNeg'])$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtPos'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtPos'])$p.value,
  NA,
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNorm'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtNorm'])$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNeg'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtNeg'])$p.value,
  wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNorm'] ~ 
                price_levels$treatment[price_levels$treatment != 'FullExtNorm'])$p.value,
  NA
)

da_price_levels_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_price_levels_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_price_levels_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Price levels Wilcoxon.html", 
          rownames = T)


##################
# Micro Level ####
##################

# aggressiveness: buyers ####

gg_aggressiveness_buyers <- ggplot(aggressiveness[aggressiveness$side == 'Buyer',], 
                                   aes(x = treatment, y = `relative aggressiveness`)) + 
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  
  labs(x = 'Treatment', y = 'Relative Aggressiveness') + theme_master()

gg_aggressiveness_buyers

ggsave("Visuals/Aggressiveness Buyers.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness Buyers.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test to get p-values and find out whether differences are statistically significant
# build three rows to get like covariance matrix where diagonals are NA

`Control Group` <- c(
  NA, 
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'less')$p.value,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'less')$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'less')$p.value,
  NA,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNorm'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNorm'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'two.sided')$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'less')$p.value,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNorm'
                                                       & aggressiveness$side == 'Buyer'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNorm'
                                         & aggressiveness$side == 'Buyer'],
              alternative = 'two.sided')$p.value,
  NA
)

da_aggressiveness_buyers_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_aggressiveness_buyers_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_aggressiveness_buyers_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Aggressiveness Buyers Wilcoxon.html", 
          rownames = T)

# aggressiveness: sellers ####

gg_aggressiveness_sellers <- ggplot(aggressiveness[aggressiveness$side == 'Seller',], 
                                   aes(x = treatment, y = `relative aggressiveness`)) + 
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  scale_y_continuous(labels = scales::percent) + 
  
  labs(x = 'Treatment', y = 'Relative Aggressiveness') + theme_master()

gg_aggressiveness_sellers

ggsave("Visuals/Aggressiveness sellers.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness sellers.png", 
       width = 34.3, height = 22, units = "cm")

# wilcoxon test to get p-values and find out whether differences are statistically significant
# build three rows to get like covariance matrix where diagonals are NA

`Control Group` <- c(
  NA, 
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'less')$p.value,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'less')$p.value
)

`Negative Ext. Treatment` <- c(
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'less')$p.value,
  NA,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNorm'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNorm'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'two.sided')$p.value
  
)

`Positive Ext. Treatment` <- c(
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'less')$p.value,
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNorm'
                                                       & aggressiveness$side == 'Seller'] ~ 
                aggressiveness$treatment[aggressiveness$treatment != 'FullExtNorm'
                                         & aggressiveness$side == 'Seller'],
              alternative = 'two.sided')$p.value,
  NA
)

da_aggressiveness_sellers_wilcox <- rbind(`Control Group`, `Negative Ext. Treatment`, `Positive Ext. Treatment`) %>% 
  
  as.data.frame() %>% round(., 4)

colnames(da_aggressiveness_sellers_wilcox) <- c('Control Group', 'Negative Ext. Treatment', 'Positive Ext. Treatment')

stargazer(da_aggressiveness_sellers_wilcox, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Aggressiveness sellers Wilcoxon.html", 
          rownames = T)

# average aggressiveness #####

avg_aggressiveness <- trading %>%
  
  mutate(
    aggressivness = abs(valuation - bid),
    rel_aggressiveness = aggressivness / valuation
  ) %>%
  
  group_by(treatment, side) %>%
  
  summarise(
    avg_aggressiveness = mean(aggressivness, na.rm = T),
    avg_rel_aggressivess = mean(rel_aggressiveness, na.rm = T)
  ) %>%
  
  ungroup()

# aggressiveness by round ####

aggressiveness_rounds <- aggressiveness %>%
  
  mutate(
    beginning = ifelse(
      round <= 3, 'Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'
    )
  ) %>%
  
  group_by(side, treatment, beginning) %>%
  
  summarise(
     avg_relative_aggressiveness = mean(`relative aggressiveness`),
     sd = sd(`relative aggressiveness`)
  ) %>%
  
  ungroup() %>%
  
  mutate(
    upper_bound = avg_relative_aggressiveness + (sd*1.96),
    lower_bound = ifelse(avg_relative_aggressiveness - (sd*1.96) < 0,
                         0,
                         avg_relative_aggressiveness - (sd*1.96)),
    beginning = factor(beginning, levels = c('Start of Game (Rounds 1-3)', 'Rest (Rounds 4-10)'))
  ) 

gg_aggressiveness_buyers_point_range <- ggplot(aggressiveness_rounds[aggressiveness_rounds$side == 'Buyer',],
                                        aes(x = beginning, y = avg_relative_aggressiveness)) + 
  
  geom_pointrange(aes(y = avg_relative_aggressiveness, ymin = lower_bound, ymax = upper_bound, color = treatment), 
                  position = position_dodge2(width = .3),
                  size = .6) +
  
  scale_y_continuous(labels = scales::percent) + scale_color_brewer(palette = 'Blues') +
  
  labs(x = 'Start vs. Rest of Game', y = 'Average Relative Aggressiveness', color = 'Treatment Condition') + 
  
  theme_master()

gg_aggressiveness_buyers_point_range

ggsave("Visuals/Aggressiveness Buyers Start of Game.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness Buyers Start of Game.png", 
       width = 34.3, height = 22, units = "cm")

## sellers ##

gg_aggressiveness_sellers_point_range <- ggplot(aggressiveness_rounds[aggressiveness_rounds$side == 'Seller',],
                                               aes(x = beginning, y = avg_relative_aggressiveness)) + 
  
  geom_pointrange(aes(y = avg_relative_aggressiveness, ymin = lower_bound, ymax = upper_bound, color = treatment), 
                  position = position_dodge2(width = .3),
                  size = .6) +
  
  scale_y_continuous(labels = scales::percent) + scale_color_brewer(palette = 'Blues') +
  
  labs(x = 'Start vs. Rest of Game', y = 'Average Relative Aggressiveness', color = 'Treatment Condition') + 
  
  theme_master()

gg_aggressiveness_sellers_point_range

ggsave("Visuals/Aggressiveness Sellers Start of Game.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness Sellers Start of Game.png", 
       width = 34.3, height = 22, units = "cm")

