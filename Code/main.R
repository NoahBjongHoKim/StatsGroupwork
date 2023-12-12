library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)

setwd('/Users/noahkim/Documents/Studium/ISTP/Stats 1/Groupwork/StatsGroupwork/')
directory <- getwd()

#filter data and create datasets East_Ger, West_Ger and Berlin
source('filter_Data.R')

# N for DE: 2358
# N >= 18: 2273
# N for West_Ger: 1807
# N for East_Ger: 376
# N for Berlin: 90

plot_distribution_histograms <- function(dataframe, name_index) {
  # Plot histograms of individual trust indicators to see the distribution
  file_directory <- paste0(directory, "/Distribution histograms/")
  ggplot(dataframe, aes(x = factor(trstplt))) + 
    geom_bar(fill = "indianred3") + 
    labs(title="Trust in politicians",
         x = "Response",
         y = "Count")
  file_name = paste0(file_directory, name_index," Trust in politicians.png" )
  ggsave(file_name, width = 6, height = 5)
  
  ggplot(dataframe, aes(x = factor(trstprt))) + 
    geom_bar(fill = "dodgerblue3") + 
    labs(title="Trust in parties",
         x = "Response",
         y = "Count")
  file_name = paste0(file_directory, name_index," Trust in parties.png" )
  ggsave(file_name, width = 6, height = 5)  
  
  ggplot(dataframe, aes(x = factor(trstprl))) + 
    geom_bar(fill = "darkseagreen3") + 
    labs(title = "Trust in parliament",
         x = "Response",
         y = "Count")
  file_name = paste0(file_directory, name_index," Trust in parliament.png" )
  ggsave(file_name, width = 6, height = 5)  
  
  # Compute Cronbach's Alpha (result = 0.884, so quite good!)
  filtered_dataframe <- dataframe %>% 
    dplyr::select(trstprl, trstplt, trstprt)
  c_alpha <- cronbach.alpha(filtered_dataframe)
  
  
  # Display the stacked Trust
  filtered_dataframe_long <- gather(filtered_dataframe, key = "Variable", value = "Response", trstprl:trstprt)
  
  custom_colors <- c("trstprl" = "darkseagreen3", "trstplt" = "indianred3", "trstprt" = "dodgerblue3")
  custom_labels <- c("trstprl" = "Trust in Parliament", "trstplt" = "Trust in Politicians", "trstprt" = "Trust in Parties")
  
  ggplot(filtered_dataframe_long, aes(x = Response, fill = Variable)) +
    geom_bar(aes(fill = Variable)) +
    scale_fill_manual(values = custom_colors, labels = custom_labels) +
    labs(title = paste0("Distribution of responses to questions of trust in government, c_alpha =",c_alpha),
         x = "Response",
         y = "Count")
  file_name = paste0(file_directory, name_index," Total Trust.png" )
  ggsave(file_name, width = 7, height = 5)}

# All of Germany:
# We can filter out all the rows which have one or more NaNs in trstplr, trstplt, trstprt
filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt), all_vars(!is.na(.)))

# Stack the trust variables
filt_trust <- filt_trust %>%
  mutate(total_trust = trstprl + trstplt + trstprt)

plot_distribution_histograms(filt_trust, "Whole Germany")
