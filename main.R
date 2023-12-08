library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)

load("ESS_2018_CH.Rda")
ess <- ESS9
de <- ESS9 %>%
  filter(cntry == "DE") %>%
  filter(agea >= 18)

# N for DE: 2358
# N >= 18: 2273

plt <- de$trstplt
prt <- de$trstprt
prl <- de$trstprl

overall_trust <- data.frame(TrustPrl = prl, TrustPlt = plt, TrustPrt = prt)

# We can filter out all the rows which have one or more NaNs.
filt_trust <- overall_trust[complete.cases(overall_trust), ] 

# Plot histograms to see the distribution
ggplot(filt_trust, aes(x = TrstPlt)) + 
  geom_histogram() + 
  labs(title="Trust in politicians")

ggplot(filt_trust, aes(x = TrstPrt)) + 
  geom_histogram() + 
  labs(title="Trust in parties")

ggplot(filt_trust, aes(x = TrstPrl)) + 
  geom_histogram() + 
  labs(title = "Trust in parliament")

# Compute Cronbach's Alpha (result = 0.884, so quite good!)
cronbach.alpha(filt_trust)