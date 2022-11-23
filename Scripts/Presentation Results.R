source('Setup/Init.R')
source('Setup/theme_master.R')
# macro Level ####

##################

# trading volume
tv_sum_nor <- 
  # I only need summary statistic 3 (= Median) & 4 (Mean)
  summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
tv_sum_pos <- 
  summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
tv_sum_neg <- 
  summary(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])
tv_sd_neg <- 
  sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])
tv_sd_pos <-
  sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
tv_sd_nor <-
  sd(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
tv_n_nor <- 
  length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNorm'])
tv_n_pos <-
  length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtPos'])
tv_n_neg <-
  length(trading_volume$`Possible Trades %`[trading_volume$treatment == 'FullExtNeg'])

# make sure that levels are correct

trading_volume$treatment <- factor(
  trading_volume$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!
# usually tests whether median an distribution are the same
# alternative hypothesis tests whether median is greater (i.e. to the right) or smaller 
# (i.e. to the left) of the first distribution and median

# the wilcox actually tests whether the median of the difference between group A and group B
# is different from 0

# positive ext (np = norm ~ positive)
tv_wil_np <- 
  wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtNeg'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtNeg'], 
            alternative = 'l')$p.value

# negative ext (nn = norm ~ negative)
tv_wil_nn <- wilcox.test(trading_volume$`Possible Trades %`[trading_volume$treatment != 'FullExtPos'] ~ 
              trading_volume$treatment[trading_volume$treatment != 'FullExtPos'], 
            alternative = 'g')$p.value

# everything in one table
# row by row and finally one rbind

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  tv_n_nor,
  # mean
  round(tv_sum_nor[4], 2),
  # median
  round(tv_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  tv_n_neg,
  # mean
  round(tv_sum_neg[4], 2),
  # median
  round(tv_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' > 0' ),
  # p-value
  round(tv_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  tv_n_pos,
  # mean
  round(tv_sum_pos[4], 2),
  # median
  round(tv_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(tv_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
    'Treatment', 
    'No. of Observations',
    'Mean',
    'Median',
    'Alternative Hypothesis',
    'P-value'
  )

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Trading Volume Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)

##################

# gains of trade
got_sum_nor <-
  summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
got_sum_pos <-
  summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
got_sum_neg <-
  summary(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])
got_sd_neg <-
  sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])
got_sd_pos <-
  sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
got_sd_nor <-
  sd(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
got_n_nor <-
  length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNorm'])
got_n_pos <-
  length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtPos'])
got_n_neg <-
  length(surplus$`Allocative efficiency`[surplus$treatment == 'FullExtNeg'])

# make sure that levels are correct

surplus$treatment <- factor(
  surplus$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
got_wil_np <-
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtNeg'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtNeg'], 
            alternative = 'g')$p.value

# negative ext
got_wil_nn <- 
  wilcox.test(surplus$`Allocative efficiency`[surplus$treatment != 'FullExtPos'] ~ 
              surplus$treatment[surplus$treatment != 'FullExtPos'], 
            alternative = 'g')$p.value

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  got_n_nor,
  # mean
  round(got_sum_nor[4], 2),
  # median
  round(got_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  got_n_neg,
  # mean
  round(got_sum_neg[4], 2),
  # median
  round(got_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' > 0' ),
  # p-value
  round(got_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  got_n_pos,
  # mean
  round(got_sum_pos[4], 2),
  # median
  round(got_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(got_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
  'Treatment', 
  'No. of Observations',
  'Mean',
  'Median',
  'Alternative Hypothesis',
  'P-value'
)

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Gains of Trade Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)

##################

# price volatility
vol_sum_nor <-
  summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
vol_sum_pos <-
  summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
vol_sum_neg<-
  summary(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])
vol_sd_neg <-
  sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])
vol_sd_pos <-
  sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
vol_sd_nor <-
  sd(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
vol_n_nor <-
  length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNorm'])
vol_n_pos <-
  length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtPos'])
vol_n_neg <-
  length(price_volatility$`price_volatility`[price_volatility$treatment == 'FullExtNeg'])

# make sure that levels are correct

price_volatility$treatment <- factor(
  price_volatility$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
vol_wil_np <-
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtNeg'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtNeg'], 
            alternative = 'l')$p.value

# negative ext
vol_wil_nn <- 
  wilcox.test(price_volatility$`price_volatility`[price_volatility$treatment != 'FullExtPos'] ~ 
              price_volatility$treatment[price_volatility$treatment != 'FullExtPos'], 
            alternative = 'g')$p.value
# everything in one table
# row by row and finally one rbind

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  vol_n_nor,
  # mean
  round(vol_sum_nor[4], 2),
  # median
  round(vol_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  vol_n_neg,
  # mean
  round(vol_sum_neg[4], 2),
  # median
  round(vol_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' > 0' ),
  # p-value
  round(vol_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  vol_n_pos,
  # mean
  round(vol_sum_pos[4], 2),
  # median
  round(vol_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(vol_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
  'Treatment', 
  'No. of Observations',
  'Mean',
  'Median',
  'Alternative Hypothesis',
  'P-value'
)

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Price Volatility Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)
################

# price levels
lev_sum_nor <-
  summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
lev_sum_pos <-
  summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
lev_sum_neg <-
  summary(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])
lev_sd_neg <-
  sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])
lev_sd_pos <-
  sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
lev_sd_nor <- 
  sd(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
lev_n_nor <- 
  length(price_levels$`price_levels`[price_levels$treatment == 'FullExtNorm'])
lev_n_pos <- 
  length(price_levels$`price_levels`[price_levels$treatment == 'FullExtPos'])
lev_n_neg <- 
  length(price_levels$`price_levels`[price_levels$treatment == 'FullExtNeg'])

# make sure that levels are correct

price_levels$treatment <- factor(
  price_levels$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
lev_wil_np <- wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtNeg'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtNeg'], 
            alternative = 'l')$p.value

# negative ext
lev_wil_nn <- wilcox.test(price_levels$`price_levels`[price_levels$treatment != 'FullExtPos'] ~ 
              price_levels$treatment[price_levels$treatment != 'FullExtPos'], 
            alternative = 'l')$p.value

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  lev_n_nor,
  # mean
  round(lev_sum_nor[4], 2),
  # median
  round(lev_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  lev_n_neg,
  # mean
  round(lev_sum_neg[4], 2),
  # median
  round(lev_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(lev_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  lev_n_pos,
  # mean
  round(lev_sum_pos[4], 2),
  # median
  round(lev_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(lev_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
  'Treatment', 
  'No. of Observations',
  'Mean',
  'Median',
  'Alternative Hypothesis',
  'P-value'
)

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Price Levels Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)

#######################

# micro level #########

#######################

# aggressiveness

# buyers
bag_sum_nor <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                                   aggressiveness$side == 'Buyer'])
bag_sum_pos <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                                   aggressiveness$side == 'Buyer'])
bag_sum_neg <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                                   aggressiveness$side == 'Buyer'])
bag_sd_neg <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                              aggressiveness$side == 'Buyer'])
bag_sd_pos <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                              aggressiveness$side == 'Buyer'])
bag_sd_nor <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                              aggressiveness$side == 'Buyer'])
bag_n_nor <-
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                                  aggressiveness$side == 'Buyer'])
bag_n_pos <-
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                                  aggressiveness$side == 'Buyer'])
bag_n_neg <- 
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                                  aggressiveness$side == 'Buyer'])

# make sure that levels are correct

aggressiveness$treatment <- factor(
  aggressiveness$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
bag_wil_np <- 
  wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg' & 
                                                       aggressiveness$side == 'Buyer'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg' & 
                                         aggressiveness$side == 'Buyer'], 
            alternative = 'g')$p.value

# negative ext
bag_wil_nn <- wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos' & 
                                                       aggressiveness$side == 'Buyer'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos' & 
                                         aggressiveness$side == 'Buyer'], 
            alternative = 'g')$p.value

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  bag_n_nor,
  # mean
  round(bag_sum_nor[4], 2),
  # median
  round(bag_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  bag_n_neg,
  # mean
  round(bag_sum_neg[4], 2),
  # median
  round(bag_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' > 0' ),
  # p-value
  round(bag_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  bag_n_pos,
  # mean
  round(bag_sum_pos[4], 2),
  # median
  round(bag_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' > 0' ),
  # p-value
  round(bag_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
  'Treatment', 
  'No. of Observations',
  'Mean',
  'Median',
  'Alternative Hypothesis',
  'P-value'
)

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Buyers Aggressiveness Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)

########

# aggressiveness

# sellers
sag_sum_nor <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                                   aggressiveness$side == 'Seller'])
sag_sum_pos <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                                   aggressiveness$side == 'Seller'])
sag_sum_neg <-
  summary(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                                   aggressiveness$side == 'Seller'])
sag_sd_neg <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                              aggressiveness$side == 'Seller'])
sag_sd_pos <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                              aggressiveness$side == 'Seller'])
sag_sd_nor <-
  sd(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                              aggressiveness$side == 'Seller'])
sag_n_nor <-
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNorm' & 
                                                  aggressiveness$side == 'Seller'])
sag_n_pos <-
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtPos' & 
                                                  aggressiveness$side == 'Seller'])
sag_n_neg <-
  length(aggressiveness$`relative aggressiveness`[aggressiveness$treatment == 'FullExtNeg' & 
                                                  aggressiveness$side == 'Seller'])

# make sure that levels are correct

aggressiveness$treatment <- factor(
  aggressiveness$treatment, levels = c('FullExtPos', 'FullExtNeg', 'FullExtNorm')
)
# the alternative hypothesis refers always to the first level of the variable!

# positive ext
sag_wil_np <- wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtNeg' & 
                                                       aggressiveness$side == 'Seller'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtNeg' & 
                                         aggressiveness$side == 'Seller'], 
            alternative = 'l')$p.value

# negative ext
sag_wil_nn <- wilcox.test(aggressiveness$`relative aggressiveness`[aggressiveness$treatment != 'FullExtPos' & 
                                                       aggressiveness$side == 'Seller'] ~ 
              aggressiveness$treatment[aggressiveness$treatment != 'FullExtPos' & 
                                         aggressiveness$side == 'Seller'], 
            alternative = 'l')$p.value

norm <- c(
  # Treatment
  'Control',
  # No. of Observations
  sag_n_nor,
  # mean
  round(sag_sum_nor[4], 2),
  # median
  round(sag_sum_nor[3], 2),
  # alternative hypothesis
  NA,
  # p-value
  NA
) %>% t()

neg <- c(
  # Treatment
  'Negative Externalities',
  # No. of Observations
  sag_n_neg,
  # mean
  round(sag_sum_neg[4], 2),
  # median
  round(sag_sum_neg[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(sag_wil_nn, 2)
) %>% t()

pos <- c(
  # Treatment
  'Positive Externalities',
  # No. of Observations
  sag_n_pos,
  # mean
  round(sag_sum_pos[4], 2),
  # median
  round(sag_sum_pos[3], 2),
  # alternative hypothesis
  paste0(expression('H'['A']),': ', expression('Difference'['Location']), ' < 0' ),
  # p-value
  round(sag_wil_np, 2)
) %>% t()

table <- rbind(
  norm,
  neg,
  pos
)

colnames(table) <- c(
  'Treatment', 
  'No. of Observations',
  'Mean',
  'Median',
  'Alternative Hypothesis',
  'P-value'
)

stargazer(table, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Sellers Aggressiveness Wilcoxon - Updated.html", 
          rownames = F,
          colnames = T)

# truncate aggressiveness to get a better graph
aggressiveness_trunc <- aggressiveness %>%
  
  mutate(
    truncated_aggressiveness = ifelse(
      aggressiveness$`relative aggressiveness` > 1, 1, aggressiveness$`relative aggressiveness`
    )
  )

gg_aggressiveness_sellers_trunc <- 
  
  ggplot(aggressiveness_trunc[aggressiveness_trunc$side == 'Seller',], 
                                    aes(x = treatment, y = truncated_aggressiveness)) + 
  
  geom_boxplot(width = 0.3) + geom_point(alpha = .5, color = 'navyblue') + 
  
  scale_y_continuous(labels = scales::percent) + 
  
  labs(x = 'Treatment', y = 'Relative Aggressiveness (truncated)') + theme_master()

gg_aggressiveness_sellers_trunc

ggsave("Visuals/Aggressiveness sellers truncated.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Aggressiveness sellers truncated.svg", 
       width = 34.3, height = 22, units = "cm", 
       device = CairoSVG)
