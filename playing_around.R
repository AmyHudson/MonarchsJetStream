#Keaton's exploration of the monarch stuff
#keatonwilson@me.com
#2018-08-09

library(tidyverse)
#reading in the appropriate data
bf = read_tsv("./Butterflies.txt")
jet = read_tsv("./JetIndices.txt")

#subsetting jet to match butterfly 
jet = jet %>%
  filter(YEAR > 1994)

#looking at bf data
bf %>%
  ggplot(aes(x = Year, y = MexicoArea)) +
  geom_line() +
  theme_classic()

#Removing the linear trend
bf_lm = lm(MexicoArea ~ Year, bf)
summary(bf_lm)

bf$residuals = bf_lm$residuals
bf %>%
  ggplot(aes(x = Year, y = residuals)) +
  geom_line() +
  theme_classic()

bf = bf %>%
  filter(Year <= 2012)

master = bind_cols(jet, bf) %>%
  select(-Year)

#Do we need to remove large-scale trends from jet?
master_long = master %>%
  gather(key = "jet_type", value = "value", JF_Reg1:ON_Reg9)

ggplot(master_long, aes(x = YEAR, y = value, color = jet_type)) +
  geom_line(aes(group = jet_type)) +
  theme_classic()
#Nerp - doesn't look like we need to detrend this
#Let's graph it first. 

ggplot(master_long, aes(x = value, y = residuals, color = jet_type)) +
  geom_point() +
  theme_classic() +
  geom_smooth(aes(group = 1))


lm_full = lm(residuals ~ value * jet_type, data = master_long)
summary(lm_full)

#jet_typeAM_REG2, AM_REG5, JA_REG4
master_long_sub = master_long %>%
  filter(jet_type == "AM_Reg5" | jet_type == "JA_Reg4")

lm_sub = lm(residuals ~ value * jet_type, data = master_long_sub)
summary(lm_sub)

lm_sub_1 = lm(residuals ~ value + jet_type, data = master_long_sub)
summary(lm_sub_1)

lm_simple = lm(residuals ~ value, data = master_long_sub)
summary(lm_simple)

ggplot(master_long_sub, aes(x = value, y = residuals, color = jet_type)) +
  geom_point(size = 3) +
  theme_classic() +
  geom_smooth(method = "lm") +
  labs(x = "Zonal Wind Speed", y = "Detrended monarch population numbers")
