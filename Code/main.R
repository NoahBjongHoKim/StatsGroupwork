library(dplyr)
library(tidyverse)
library(tidyr)
library(ltm)
# N for DE: 2358
# N >= 18: 2273
# N for West_Ger: 1807
# N for East_Ger: 376
# N for Berlin: 90

#setwd('/Users/noahkim/Documents/Studium/ISTP/Stats 1/Groupwork/StatsGroupwork/')
setwd('/Users/max/Documents/ETHZ/MSc_STP/Statistics_I/Project/StatsGroupwork2/StatsGroupwork')
directory <- getwd()

#filter data and create datasets East_Ger, West_Ger and Berlin
source('Code/filter_Data.R')

############################# Fuctions
plot_distribution_histograms <- function(dataframe, name_index) {
  # Plot histograms of individual trust indicators to see the distribution
  file_directory <- paste0(directory, "/Distribution histograms/")
  ggplot(dataframe, aes(x = factor(trstplt))) + 
    geom_bar(fill = "indianred3") + 
    labs(title="Trust in politicians",
         x = "Response",
         y = "Count") +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))
  file_name = paste0(file_directory,"Trust in politicians ",name_index,".png" )
  ggsave(file_name, width = 6, height = 5)
  
  ggplot(dataframe, aes(x = factor(trstprt))) + 
    geom_bar(fill = "dodgerblue3") + 
    labs(title="Trust in parties",
         x = "Response",
         y = "Count") +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))
  file_name = paste0(file_directory,"Trust in parties ",name_index,".png" )
  ggsave(file_name, width = 6, height = 5)  
  
  ggplot(dataframe, aes(x = factor(trstprl))) + 
    geom_bar(fill = "darkseagreen3") + 
    labs(title = "Trust in parliament",
         x = "Response",
         y = "Count") +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))
  file_name = paste0(file_directory,"Trust in parliament ",name_index,".png" )
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
    # labs(title = paste0("Distribution of responses to questions of trust in government, c_alpha =",c_alpha),
    #      x = "Response",
    #      y = "Count") +
    labs(title = "Total trust in Government",
         x = "Response",
         y = "Count") +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24),
          legend.text=element_text(size=20), #change font size of legend text
          legend.title=element_text(size=20)) #change font size of legend title)
  file_name = paste0(file_directory,"Total Trust ",name_index,".png" )
  ggsave(file_name, width = 8, height = 5)
  }

plot_scatterplots <- function(dataframe, name_index){
  file_directory <- paste0(directory, "/Scatterplots/")

  # Create a dataframe with the frequency of each value in the 'x' column
  frequency_data <- dataframe %>%
    group_by(polintr,total_trust) %>%
    summarize(freq = n())
  # Using ggplot2 with size based on frequency
  ggplot(frequency_data, aes(x = polintr, y = total_trust, size = freq)) +
    geom_point() +
    labs(title = "Scatterplot with Point Size based on Frequency", x = "Political Interest", y = "Total Trust") +
    scale_size_continuous(range = c(1, 8))  # Adjust the size range as needed
  file_name = paste0(file_directory,"Political Interest Scatterplot ",name_index,".png" )
  ggsave(file_name, width = 8, height = 5)
  
  # Create a dataframe with the frequency of each value in the 'x' column
  frequency_data <- dataframe %>%
    group_by(prtvede2,total_trust) %>%
    summarize(freq = n())
  # Using ggplot2 with size based on frequency
  ggplot(frequency_data, aes(x = prtvede2, y = total_trust, size = freq)) +
    geom_point() +
    labs(title = "Scatterplot with Point Size based on Frequency", x = "Party affiliation", y = "Total Trust") +
    scale_size_continuous(range = c(1, 8)) +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))  # Adjust the size range as needed
  file_name = paste0(file_directory,"Party Affiliation Scatterplot ",name_index,".png" )
  ggsave(file_name, width = 8, height = 5)
  
  # Create a dataframe with the frequency of each value in the 'x' column
  frequency_data <- dataframe %>%
    group_by(agea,total_trust) %>%
    summarize(freq = n())
  # Using ggplot2 with size based on frequency
  ggplot(frequency_data, aes(x = agea, y = total_trust, size = freq)) +
    geom_point() +
    labs(title = "Scatterplot with Point Size based on Frequency", x = "Age", y = "Total Trust") +
    scale_size_continuous(range = c(1, 8)) +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))  # Adjust the size range as needed
  file_name = paste0(file_directory,"Age Scatterplot ",name_index,".png" )
  ggsave(file_name, width = 8, height = 5)
  
  # Create a dataframe with the frequency of each value in the 'x' column
  frequency_data <- dataframe %>%
    group_by(hinctnta,total_trust) %>%
    summarize(freq = n())
  # Using ggplot2 with size based on frequency
  ggplot(frequency_data, aes(x = hinctnta, y = total_trust, size = freq)) +
    geom_point() +
    labs(title = "Scatterplot with Point Size based on Frequency", x = "Household Income", y = "Total Trust") +
    scale_size_continuous(range = c(1, 8)) +
    theme(axis.text=element_text(size=20),
          axis.title = element_text(size=20),
          plot.title = element_text(size=24))  # Adjust the size range as needed
  file_name = paste0(file_directory,"Household Income Scatterplot ",name_index,".png" )
  ggsave(file_name, width = 8, height = 5)
}

trust_regression_model <- function(dataframe){
  # Create a formula for the regression
  formula <- total_trust ~ hinctnta + polintr + prtvede2 + agea
  
  # Fit the multivariate regression model
  model <- lm(formula,dataframe)
  
  # Print the summary of the regression model
  model_summary <- summary(model)
  
  # Extract the r squared value
  r_squared_value <- model_summary$r.squared
  
  return(r_squared_value)
}

############################# Data Subset Creation
# We can filter out all the rows which have one or more NaNs in trstplr, trstplt, trstprt
Whole_DE_filt_trust <- Data_DE %>% filter_at(vars(trstprl, trstplt, trstprt, polintr, prtvede2, hinctnta, agea), all_vars(!is.na(.)))

# Create regional Subsets
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

############################# Distribution histograms/stats
# # Plot distributions of the individual and stacked trust variables
# plot_distribution_histograms(Whole_DE_filt_trust, "Whole Germany")
plot_distribution_histograms(East_DE_filt_trust, "East Germany")
# plot_distribution_histograms(West_DE_filt_trust, "West Germany")
# plot_distribution_histograms(Berlin_DE_filt_trust, "Berlin")
plot_distribution_histograms(East_DE_filt_trust, "West Germany + Berlin")
# 
# # Print summaries of the statistics of the total trust variable: East Germany has indeed lower trust
# print(summary(Whole_DE_filt_trust$total_trust))
# print(summary(West_DE_filt_trust$total_trust))
print(summary(East_DE_filt_trust$total_trust))
# print(summary(Berlin_DE_filt_trust$total_trust))
print(summary(West_Berlin_DE_filt_trust$total_trust))

# ############################ Scatterplots
# plot_scatterplots(Whole_DE_filt_trust, "Whole Germany")

############################# Regression

############################# Cronbach's alpha
trust_var_all <- Whole_DE_filt_trust %>% dplyr::select(trstplt, trstprl, trstprt)
trust_var_East <- East_DE_filt_trust %>% dplyr::select(trstplt, trstprl, trstprt)
trust_var_West <- West_DE_filt_trust %>% dplyr::select(trstplt, trstprl, trstprt)
trust_var_Berlin <- Berlin_DE_filt_trust %>% dplyr::select(trstplt, trstprl, trstprt)
trust_var_West_Berlin <- West_Berlin_DE_filt_trust %>% dplyr::select(trstplt, trstprl, trstprt)
print("Cronbach's alpha:")
cat("Whole DE: ", cronbach.alpha(trust_var_all)$alpha)
cat("East: ", cronbach.alpha(trust_var_East)$alpha)
cat("West: ", cronbach.alpha(trust_var_West)$alpha)
cat("Berlin: ", cronbach.alpha(trust_var_Berlin)$alpha)
cat("West and Berlin: ", cronbach.alpha(trust_var_West_Berlin)$alpha)

########################## Plot total trust vs polintr
East_by_polintr <- East_DE_filt_trust %>%
  group_by(polintr) %>% 
  summarize(meanAverageTrust = mean(mean_trust))
West_Berlin_by_polintr <- West_Berlin_DE_filt_trust %>%
  group_by(polintr) %>% 
  summarize(meanAverageTrust = mean(mean_trust))
ggplot(East_by_polintr, aes(x = polintr, y = meanAverageTrust)) +
  geom_col() +
  labs(title ='East Germany' , x = 'Interest in Politics', y = 'Mean of Average Trust') +
  theme(axis.text=element_text(size=20),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24))  # Adjust the size range as needed
  
ggplot(West_Berlin_by_polintr, aes(x = polintr, y = meanAverageTrust)) +
  geom_col() +
  labs(title ='West Germany and Berlin' , x = 'Interest in Politics', y = 'Mean of Average Trust') +
  theme(axis.text=element_text(size=20),
      axis.title = element_text(size=20),
      plot.title = element_text(size=24))  # Adjust the size range as needed
  


