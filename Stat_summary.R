library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)

#setwd('/Users/noahkim/Documents/Studium/ISTP/Stats 1/Groupwork/StatsGroupwork/')
setwd('/Users/max/Documents/ETHZ/MSc_STP/Statistics_I/Project/StatsGroupwork2/StatsGroupwork')
directory <- getwd()

#filter data and create datasets East_Ger, West_Ger and Berlin
source('Code/filter_Data.R')

#####Code
# We can filter out all the rows which have one or more NaNs in trstplr, trstplt, trstprt
#filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt), all_vars(!is.na(.)))
Whole_DE_filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt, polintr, lrscale, hinctnta, agea), all_vars(!is.na(.)))

means <- Whole_DE_filt_trust %>%
  group_by(EastWest) %>%
  mutate(EastWest = factor(EastWest, levels = c("East", "West", "Berlin"))) %>% 
  summarize(meanTrstprl = mean(trstprl),
            sdTrstprl = sd(trstprl),
            meanTrstplt = mean(trstplt),
            sdTrstplt = sd(trstplt),
            meanTrstprt = mean(trstprt),
            sdTrstprt = sd(trstprt))

#####Significance Tests
trust_sign %>% gather(key = variable, value = value, -EastWest) %>% 
  group_by(EastWest, variable) %>%
  summarize(value = list(value)) %>%
  spread(EastWest, value) %>%
  group_by(variable) %>% 
  mutate(p_value_East_West = t.test(unlist(East), unlist(West))$p.value,
          p_value_West_Berlin = t.test(unlist(West), unlist(Berlin))$p.value,
         p_value_East_Berlin = t.test(unlist(East), unlist(Berlin))$p.value)

#####Plots

ggplot(means, aes(x = EastWest, y = meanTrstplt)) +
  geom_col(fill = "indianred3") +
  #geom_errorbar(aes(x = EastWest, ymax = meanTrstprl + sdTrstprl, ymin = meanTrstprl - sdTrstprl), width = 0.25) +
  ylim(0,10) +
  labs(title = "Trust in Politicians",
       x = '',
       y = 'Score') +
  theme(axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24))
  

ggplot(means, aes(x = EastWest, y = meanTrstprl)) +
  geom_col(fill = "dodgerblue3") +
  ylim(0,10) +
  labs(title = "Trust in Parliament",
       x = '',
       y = 'Score') +
  theme(axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24))

ggplot(means, aes(x = EastWest, y = meanTrstprt)) +
  geom_col(fill = "darkseagreen3") +
  ylim(0,10) +
  labs(title = "Trust in Political Parties",
       x = '',
       y = 'Score') +
  theme(axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24))


