library(tidyverse)
library(haven)

#load ESS data
load('ESS9e03_2.Rda')

#create filtered dataset from Germany, respondent above age of 18
Data_DE <- ESS9 %>% filter(cntry == "DE", agea >= 18)

#load German state codes and classification
Germany_states <- read.csv('Germany_States.csv', header=TRUE)

# String vector with ESS9 codes of states in East Germany, West Germany, Berlin
Code_Ger_states_East <- Germany_states$Code[Germany_states$Region == "East"]
Code_Ger_states_West <- Germany_states$Code[Germany_states$Region == "West"]
Code_Ger_states_Berlin <- "DE3"

#Add variable if in East Germany, West Germany or Berlin
Data_DE <- Data_DE %>% 
  mutate(EastWest = case_when(region %in% Code_Ger_states_East ~ "East",
                              region %in% Code_Ger_states_West ~ "West",
                              region %in% Code_Ger_states_Berlin ~ "Berlin"))