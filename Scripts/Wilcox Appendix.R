# some additional info on the Mann-Whitney-Wilcoxon Test
# 

rm(list = ls())

source('Setup/Init.R')
source('Setup/theme_master.R')

# Examples on what the statistical test actually tests/compares
# Example with Price Levels
load('Daten/trading_behavior.rda')

# looking only at the control group and negative externalities
price_levels <- trading_behavior %>%
  
  filter(!is.na(price),
         treatment != 'FullExtPos') %>%
  
  group_by(treatment, game, round) %>%
  
  summarise(
    price_levels = mean(price, na.rm = T)
  ) %>%
  
  ungroup()

# basically the wilcoxon test compares the distributions
# by ranking the values and comparing the ranks of the two groups with each other

c_price_levels <- price_levels %>%
  
  filter(treatment == 'FullExtNorm') %>%
  
  arrange(desc(price_levels)) %>%
  
  select(treatment, price_levels) %>%
  
  mutate(
    c_rank = 1:n(),
    `Control Price Levels` = price_levels
  ) %>%
  
  select(-price_levels)

t_price_levels <- price_levels %>%
  
  filter(treatment == 'FullExtNeg') %>%
  
  arrange(desc(price_levels)) %>%
  
  select(treatment, price_levels) %>%
  
  mutate(
    t_rank = 1:n(),
    `Treatment Price Levels` = price_levels
  ) %>%
  
  select(-price_levels)

ranked <- cbind(c_price_levels, t_price_levels) %>%
  
  select(rank = c_rank, `Control Price Levels`, `Treatment Price Levels`)

stargazer(ranked, type = "html", summary = FALSE, 
          header = F, out = "Visuals/Price Levels Distribution Ranks.html", 
          rownames = F,
          colnames = T)

# the summary statistic w indicates how many times a given value of e.g. the treatment group is 
# bigger than the values from the control group. 

sum(outer(price_levels$price_levels[price_levels$treatment == 'FullExtNeg'], 
          price_levels$price_levels[price_levels$treatment == 'FullExtNorm'], '>'))

# summing up this table results then in the w summary statistic

# this code basically does the same like a loop that checks every value on the 'bigger than'
# condition in a matrix calculation like fashion; so

w <- 0

for (i in 1:length(price_levels$price_levels[price_levels$treatment == 'FullExtNeg'])) {
  
  for (j in 1:length(price_levels$price_levels[price_levels$treatment == 'FullExtNorm'])) {
    
    if(
      price_levels$price_levels[price_levels$treatment == 'FullExtNeg'][i] > 
      price_levels$price_levels[price_levels$treatment == 'FullExtNorm'][j]
    ){
      
      w <- w + 1
      
    }
    
  }
  
}

w

# when testing an alternative hypothesis, the following code prints a matrix calculation
# from which one takes the median as the point estimate for the true location shift

outer(
  price_levels$price_levels[price_levels$treatment == 'FullExtNeg'],
  price_levels$price_levels[price_levels$treatment == 'FullExtNorm'],
  '-'
) %>% median() -> pe_location_shift_price_levels

pe_location_shift_price_levels

# a negative value indicates that the distribution of the negative externality
# probably is to the left of the distribution of the control condition
# this can also be shown using a histogram with a density curve

price_levels <- price_levels %>%
  
  mutate(
    `Treatment Condition` = treatment
  )

gg_price_levels_hist <-
  
  ggplot(price_levels, 
         aes(
           x = price_levels, 
           color = `Treatment Condition`, 
           fill = `Treatment Condition`
           )
         ) +
  
  geom_histogram(aes(y = ..density..), alpha = .4, position = 'identity',
                 binwidth = .8) +
  
  geom_density(alpha = .4) + 
  
  scale_y_continuous(labels = scales::percent, limits = c(0, .2)) +
  
  geom_vline(
    data = price_levels[price_levels$treatment == 'FullExtNorm',],
    aes(xintercept = median(price_levels), color = `Treatment Condition`),
    linetype = 'dashed'
    ) + 
  
  geom_vline(
    data = price_levels[price_levels$treatment == 'FullExtNeg',],
    aes(xintercept = median(price_levels), color = `Treatment Condition`),
    linetype = 'dashed'
  ) +
  
  scale_color_brewer(palette = 'Dark2') +
  
  scale_fill_brewer(palette = 'Dark2') +
  
  labs(x = 'Price Levels', y = 'Density') +
  
  theme_master()

gg_price_levels_hist

ggsave("Visuals/Price Levels Distributional Location Shift.pdf", 
       width = 34.3, height = 22, units = "cm",
       device = cairo_pdf)

ggsave("Visuals/Price Levels Distributional Location Shift.svg", 
       width = 34.3, height = 22, units = "cm",
       device = CairoSVG)
