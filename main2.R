library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)

#filter data and create datasets East_Ger, West_Ger and Berlin
source('filter_Data.R')

# N for DE: 2358
# N >= 18: 2273
# N for West_Ger: 1807
# N for East_Ger: 376
# N for Berlin: 90

# All of Germany:

# We can filter out all the rows which have one or more NaNs in trstplr, trstplt, trstprt
filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt), all_vars(!is.na(.)))


# Plot histograms to see the distribution
ggplot(filt_trust, aes(x = factor(trstplt))) + 
  geom_bar() + 
  labs(title="Trust in politicians")

ggplot(filt_trust, aes(x = factor(trstprt))) + 
  geom_bar() + 
  labs(title="Trust in parties")

ggplot(filt_trust, aes(x = factor(trstprl))) + 
  geom_bar() + 
  labs(title = "Trust in parliament")

# Compute Cronbach's Alpha (result = 0.884, so quite good!)
trust_var <- filt_trust %>% 
  dplyr::select(trstprl, trstplt, trstprt)
cronbach.alpha(trust_var)