library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)
library(patchwork)


#filter data and create datasets East_Ger, West_Ger and Berlin
source('Code/filter_Data.R')

### Trust variable distribution ###

# We can filter out all the rows which have one or more NaNs in trstplr, trstplt, trstprt
filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt), all_vars(!is.na(.)))

#Create regional Subsets
East_DE_filt_trust <- Whole_DE_filt_trust %>% filter(EastWest == "East")
West_DE_filt_trust <- Whole_DE_filt_trust %>% filter(EastWest == "West")
Berlin_DE_filt_trust <- Whole_DE_filt_trust %>% filter(EastWest == "Berlin")
West_Berlin_DE_filt_trust <- Whole_DE_filt_trust %>% filter(EastWest == "West" | EastWest == "Berlin")

# Stack the trust variables
Whole_DE_filt_trust <- Whole_DE_filt_trust %>%
  mutate(total_trust = trstprl + trstplt + trstprt,
         mean_trust = total_trust/3)
East_DE_filt_trust <- East_DE_filt_trust %>% 
  mutate(total_trust = trstprl + trstplt + trstprt,
         mean_trust = total_trust/3)
West_Berlin_DE_filt_trust <- West_Berlin_DE_filt_trust %>% 
  mutate(total_trust = trstprl + trstplt + trstprt,
         mean_trust = total_trust/3)

#Compute descriptive stat Trust
Stat_Trust_East <- East_DE_filt_trust %>% 
  summarize(median_AverageTrust = median(mean_trust),
            mode_AverageTrust = mode(mean_trust),
            mean_AverageTrust = mean(mean_trust),
            sd_AverageTrust = sd(mean_trust),
            n_AverageTrust = n())

Stat_Trust_West <- West_Berlin_DE_filt_trust %>% 
  summarize(median_AverageTrust = median(mean_trust),
            mode_AverageTrust = mode(mean_trust),
            mean_AverageTrust = mean(mean_trust),
            sd_AverageTrust = sd(mean_trust),
            n_AverageTrust = n())

#Print descriptive stat Trust
print(Stat_Trust_East)
print(Stat_Trust_West)
#Plot Trust
# p1 = ggplot(East_DE_filt_trust, aes(x = factor(trstplt))) +
#   geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "gray85") +
#   ylim(0,0.25) +
#   #geom_vline(xintercept = Stat_Trust_East$median_AverageTrust, color = 'black') +
#   geom_vline(aes(xintercept = Stat_Trust_East$mean_AverageTrust + 1), color = 'black', linetype = 'dashed') +
#   annotate("text", x = 5, y = 0.24, label = "mean = 3.85", vjust = 0, hjust = 0) +
#   labs(title = "East",
#        x = "Trust in government",
#        y = "Frequency") +
#   theme_bw()
# p2 = ggplot(West_Berlin_DE_filt_trust, aes(x = factor(trstplt))) +
#   geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "gray50") +
#   ylim(0,0.25) +
#   #geom_vline(xintercept = Stat_Trust_West$median_AverageTrust, color = 'black') +
#   geom_vline(aes(xintercept = Stat_Trust_West$mean_AverageTrust + 1), color = 'black', linetype = 'dashed') +
#   annotate("text", x = 6, y = 0.24, label = "mean = 4.60", vjust = 0, hjust = 0) +
#   labs(title = "West and Berlin",
#        x = "Trust in government",
#        y = NULL) +
#   theme_bw()
# p1 + p2

p1 = ggplot(East_DE_filt_trust, aes(x = mean_trust)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "gray85") +
  #ylim(0,0.25) +
  #geom_vline(xintercept = Stat_Trust_East$median_AverageTrust, color = 'black') +
  geom_vline(aes(xintercept = Stat_Trust_East$mean_AverageTrust), color = 'black', linetype = 'dashed') +
  annotate("text", x = 5, y = 0.24, label = "mean = 3.85", vjust = 0, hjust = 0) +
  labs(title = "East",
       x = "Trust in government",
       y = "Frequency") +
  theme_bw()
p2 = ggplot(West_Berlin_DE_filt_trust, aes(x = mean_trust)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "gray50") +
  #ylim(0,0.25) +
  #geom_vline(xintercept = Stat_Trust_West$median_AverageTrust, color = 'black') +
  geom_vline(aes(xintercept = Stat_Trust_West$mean_AverageTrust), color = 'black', linetype = 'dashed') +
  annotate("text", x = 6, y = 0.24, label = "mean = 4.60", vjust = 0, hjust = 0) +
  labs(title = "West and Berlin",
       x = "Trust in government",
       y = NULL) +
  theme_bw()
p1 + p2





### Political interest distribution ###

#filter na's in polintr and flip polintr
filt_polintr <- Data_DE %>%
  filter(!is.na(polintr)) %>%
  mutate(polintr = case_when(
    polintr == 1 ~ 4,
    polintr == 2 ~ 3,
    polintr == 3 ~ 2,
    polintr == 4 ~ 1
  ))

#Create regional subsets
East_DE_filt_polintr <- filt_polintr %>% filter(EastWest == "East")
West_DE_filt_polintr <- filt_polintr %>% filter(EastWest == "West" | EastWest == "Berlin")

#Compute descriptive stat
Stat_polintr_East <- East_DE_filt_polintr %>%
  summarize(meanPolintr = mean(polintr),
            medianPolintr = median(polintr),
            sdPolintr = sd(polintr),
            nPolintr = n())

Stat_polintr_West <- West_DE_filt_polintr %>%
  summarize(meanPolintr = mean(polintr),
            medianPolintr = median(polintr),
            sdPolintr = sd(polintr),
            nPolintr = n())
#Print descriptive stat
print(Stat_polintr_East)
print(Stat_polintr_West)

#Plot polintr distribution
p3 = ggplot(East_DE_filt_polintr, aes(x = factor(polintr))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "gray85") +
  geom_vline(xintercept = Stat_polintr_East$meanPolintr, color = 'black', linetype = 'dashed') +
  ylim(0,0.5) +
  annotate("text", x = 3.75, y = 0.47, label = "mean = 2.89") +
  labs(title = "East",
     x = "Interest in politics",
     y = "Frequency") +
  theme_bw()

p4 = ggplot(West_DE_filt_polintr, aes(x = factor(polintr))) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), fill = "gray50") +
  geom_vline(xintercept = Stat_polintr_West$meanPolintr, color = 'black', linetype = 'dashed') +
  ylim(0,0.5) +
  annotate("text", x = 3.75, y = 0.47, label = "mean = 2.88") +
  labs(title = "West and Berlin",
       x = "Interest in politics",
       y = NULL) +
  theme_bw()

p3 + p4

### Correlations ###

#filter out polintr NA's and flip polintr
East_DE_filt_trust <- East_DE_filt_trust %>% 
  filter(!is.na(polintr)) %>% 
  mutate(polintr = case_when(
    polintr == 1 ~ 4,
    polintr == 2 ~ 3,
    polintr == 3 ~ 2,
    polintr == 4 ~ 1
  ))
West_Berlin_DE_filt_trust <- West_Berlin_DE_filt_trust %>% 
  filter(!is.na(polintr)) %>% 
  mutate(polintr = case_when(
    polintr == 1 ~ 4,
    polintr == 2 ~ 3,
    polintr == 3 ~ 2,
    polintr == 4 ~ 1
  ))





#Plot mean_trust vs polintr

#linear models
lm1_East = lm(mean_trust ~ polintr, East_DE_filt_trust)
lm1_West = lm(mean_trust ~ polintr, West_Berlin_DE_filt_trust)

lm2_East = lm(mean_trust ~ agea, East_DE_filt_trust)
lm2_West = lm(mean_trust ~ agea, West_DE_filt_trust)

lm3_East = lm(mean_trust ~ eduyrs, East_DE_filt_trust)
lm3_West = lm(mean_trust ~ eduyrs, West_DE_filt_trust)

lm4_West = lm(polintr ~ eduyrs, West_DE_filt_trust)

#Pearson R - East
#summary(lm1_East)
print(summary(lm1_East)$coefficients)
print(sqrt(summary(lm1_East)$r.squared))
print(summary(lm2_East)$coefficients)
print(sqrt(summary(lm2_East)$r.squared))
print(summary(lm3_East)$coefficients)
print(sqrt(summary(lm3_East)$r.squared))
#Pearson R - West
#summary(lm1_West)
print(summary(lm1_West)$coefficients)
print(sqrt(summary(lm1_West)$r.squared))
print(summary(lm2_West)$coefficients)
print(sqrt(summary(lm2_West)$r.squared))
print(summary(lm1_East)$coefficients)
print(sqrt(summary(lm3_West)$r.squared))

print(summary(lm4_West)$coefficients)
print(sqrt(summary(lm4_West)$r.squared))










  



# theme(axis.text=element_text(size=20),
# axis.title = element_text(size=20),
# plot.title = element_text(size=24)) +